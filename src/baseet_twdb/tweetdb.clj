(ns baseet-twdb.tweetdb
  (:require [com.ashafa.clutch :as db]
            [suweet.twitter :as suweet :only (get-twitter-lists
                                              get-twitter-list-tweets)]
            [suweet.score :as score :only (score-tweet)]
            [clj-time.core :as clj-time]
            [clj-time.coerce :as coerce]))

(defn build-tw-list-doc
  "Construct the twitter list documents. Add timestamp as well"
  [tw-lists]
  (map (comp #(-> %
                  (assoc :last-update-time (str (clj-time.core/now)))
                  (assoc :schema "tw-list"))
             #(zipmap [:name :list-id :since-id] %)
             (juxt :slug :id (constantly 0)))
       tw-lists))

(defn db-create-tw-list-design-doc!
  "function to programmatically add views for our lists.
  It's more performant to emit the whole doc that later use include_doc
  in the view query. This increases the size the index, but uses less I/O
  resources and results in faster document retrieval."
  [db-name doc-id view-name list-id]
  (db/save-design-document db-name :views (str doc-id)
                           ["javascript"
                            {(str view-name "-tweets")
                             {:map (str "function(doc) {if(doc['schema'] == 'tweet' &&"
                                        " doc['list-id'] == " list-id " )"
                                        " emit(doc['unique-score'],doc);}")}}])
  (db/save-design-document db-name :views (str doc-id "-unread")
                           ["javascript"
                            {(str view-name "-unread-tweets")
                             {:map (str "function(doc) {if(doc['schema'] == 'tweet' &&"
                                        " doc['list-id'] == " list-id
                                        " && doc.unread && doc.unread == true)"
                                        " emit(doc['unique-score'],doc);}")}}]))

(defn create-twitter-list-views
  "Create view for each of our twitter lists. This should help
  make our queries a lot quicker than getting all our documents"
  [db-name tw-lists]
  (doall
    (map (comp (fn [[db-name doc-id view-name list-id]]
                 (db-create-tw-list-design-doc! db-name doc-id view-name list-id))
               (juxt (constantly db-name) :name :name :list-id)) tw-lists))
  tw-lists)

(defn get-from
  "Similar to some. Except that it returns the item matching.
  getter is a function used to get the item"
  [coll getter item]
  (when (seq coll)
    (if (= (getter (first coll)) item)
      (first coll)
      (recur (next coll) getter item))))

(defn db-update-tw-lists!
  "Side-effects of updating db. This function assumes that we have
  existing entries that need to be updated because of age. There is
  also the possibility of the addition of new lists that we haven't been
  tracking before"
  [db-name existing-lists latest-lists]
  (reduce (fn [updates new-list]
            (if-let [existing-list (get-from existing-lists
                                             (comp :list-id :value)
                                             (:list-id new-list))]
              ;; an exiting twitter list, just update the timestamp
              (conj updates (as-> (:value existing-list) _
                              (assoc _ :last-update-time (str (clj-time.core/now)))
                              (assoc _ :_rev (:_rev _))
                              (assoc _ :_id (:_id _))))
              ;; else, this is a new twitter list. create view and entry
              (do
                (db-create-tw-list-design-doc! db-name
                                               (:name new-list)
                                               (:name new-list)
                                               (:list-id new-list))
                (db/put-document db-name new-list)
                updates)))
          [] latest-lists))

(defn twitter-lists-need-update?
  "Should update out twitter lists once a day. The view returns with a map
   with value containing last-update-time"
  [tw-list-views]
  (some #((complement clj-time/within?)
          (clj-time/interval (-> 1 clj-time/days clj-time/ago) (clj-time/now))
          (coerce/from-string (-> % :value :last-update-time)))
        tw-list-views))

(defn db-get-all-twitter-lists!
  "Return a map containing user's twitter lists (list name and id)
  The twitter list db is called tw-lists as well as the document itself."
  [ctx]
  (let [db-name (-> ctx :db-params :db-name)
        db-tw-lists-view (-> db-name
                             (db/get-view
                               (-> ctx :db-params :views :twitter-list :view-name)
                               (-> ctx :db-params :views :twitter-list :view-name keyword)))]
    (cond
      (empty? db-tw-lists-view)
        (let [latest-tw-lists (build-tw-list-doc
                                (suweet/get-twitter-lists (:twitter-params ctx)))]
          (->> latest-tw-lists
               (create-twitter-list-views db-name)
               (db/bulk-update db-name))
          latest-tw-lists)
      ;; nil is also a logical false in clojure
      (twitter-lists-need-update? db-tw-lists-view)
        (let [latest-tw-lists (build-tw-list-doc
                                (suweet/get-twitter-lists (:twitter-params ctx)))]
          (as-> db-name _
              (db-update-tw-lists! _ db-tw-lists-view latest-tw-lists)
              (db/bulk-update db-name _))
          latest-tw-lists)
      :else db-tw-lists-view)))

(defn generate-unique-score
  "Make a unique score string while preserving the correct order.
  This is needed because since we base our tweet scores on favs/retweets
  there is the possibility of getting 0 scores, so we need a tie-breaker.
  Use the tweet-id as the lowest significant 20 digits,
  and for the upper 8 digits, multiply the raw score by 10^8 taking
  care of rounding etc.."
  [raw-score tweet]
  (let [raw-score-factor 100000000
        max-raw-score (- raw-score-factor 1)
        score (java.lang.Math/round (double (* raw-score raw-score-factor)))]
    (format "%08d%020d"
            (if (> score max-raw-score) max-raw-score score) (:id tweet))))

(defn make-tweet-db-doc
  "Score our tweets and save them in db"
  [list-id tweets]
  (->> (:links tweets)
       (map #(let [score (score/score-tweet
                            {:tw-score :default}
                            ((juxt :fav-counts :rt-counts :follow-count) %))]
               (-> %
                   (assoc :schema "tweet")
                   (assoc :unread true)
                   (assoc :save false)
                   (assoc :list-id list-id)
                   (assoc :score score)
                   (assoc :unique-score (generate-unique-score score %)))))))

(defn too-old?
  ([tweet-activity-view] (too-old? 3 tweet-activity-view))
  ([days tweet-activity-view]
  ((complement clj-time/within?)
          (clj-time/interval (-> days clj-time/days clj-time/ago) (clj-time/now))
          (coerce/from-string tweet-activity-view))))

(defn mark-old-tweets-for-deletion
  "Get rid of : unread tweets older than 15 days and read tweets older than 3 days
  from db. Use the by-list view in order to get all the tweets from a given list"
  [db-params list-id]
  (let [db-name (:db-name db-params)
        db-tw-activity-view (-> (:db-name db-params)
                                (db/get-view
                                  (-> db-params :views :tweets :view-name)
                                  (-> db-params :views :tweets :view-name keyword)
                                  {:key (str list-id)}))]
    (if-let [old-tweets (and (seq db-tw-activity-view)
                             (seq (->> db-tw-activity-view 
                                       ;; delete all tweets older than 15 days
                                       (filter (comp (partial too-old? 15)
                                                     (comp :last-activity :value)))
                                       ;; get tweets that were marked read
                                       (filter (comp false? :unread :value))
                                       ;; delete tweets which were read older than 3 days
                                       (filter (comp too-old? (comp :last-activity :value))))))]
      (map #(as-> % _
              (assoc _ :_rev (-> _ :value :_rev))
              (assoc _ :_id (:id _))
              (assoc _ :_deleted true)) old-tweets))))

(defn db-update-since-id!
  "Side effects a-plenty. First get the list view from db containing the
  latest since-id. Get tweets since the since-id. Update db entry with
  the id obtained from the view query. Finally return the new tweets."
  [{:keys [db-params twitter-params]} list-id]
  (let [db-name (:db-name db-params)
        db-tw-lists-view (-> db-name
                             (db/get-view
                               (-> db-params :views :twitter-list :view-name)
                               (-> db-params :views :twitter-list :view-name keyword)
                               {:key (Integer. list-id)}))]
    (when (seq db-tw-lists-view)
      (let [doc-id (:id (first db-tw-lists-view))
            since-id (:since-id (db/get-document db-name doc-id))
            tweets (->> {:list-id list-id :since-id since-id}
                        (suweet/get-twitter-list-tweets twitter-params))]
        (when (> (:since-id tweets) since-id)
          (db/update-document db-name
                              (db/get-document db-name doc-id)
                              assoc :since-id (:since-id tweets))
          tweets)))))

(defn db-tweet-housekeep!
  [{:keys [db-params] :as ctx} list-id]
  (let [old-tweets (mark-old-tweets-for-deletion db-params list-id)]
    (if-let [new-tweets (some->> list-id
                                 (db-update-since-id! ctx)
                                 (make-tweet-db-doc list-id))]
      (apply conj old-tweets new-tweets)
      old-tweets)))

(defn db-update-tweets!
  "Update db with latest tweets"
  [{:keys [db-params] :as ctx} list-id]
  (let [db-name (:db-name db-params)]
    (some->> list-id
         (db-tweet-housekeep! ctx)
         (db/bulk-update db-name))))

(defn get-unread-tweets
  "Get the tweet view for the twitter list"
  [option {:keys [db-params] :as ctx} list-name view-cfg]
  (let [db-name (:db-name db-params)
        doc-id (cond-> list-name
                    (= :unread option) (str "-unread"))]
    (as-> db-name _
        (db/get-view _ doc-id (str doc-id "-tweets") view-cfg)
        (with-meta (map #(assoc % :list-name list-name) _) (meta _)))))
