+++
title = "RCRPG/Clojure"
description = ""
date = 2011-10-15T22:45:20Z
aliases = []
[extra]
id = 10676
[taxonomies]
categories = []
tags = []
+++

{{collection|RCRPG}}
[[Clojure]] version of [[:Category:RCRPG|RCRPG]]. The code can also be checked out and contributed to on [https://github.com/pistacchio/rosettacode.clojure.rcrpg github] .

==Code==

```lisp

(ns rosettacode.rcrpg.clojure)
(use '[clojure.string :only (join split)])

(defn split-keys
  "returns map m (with string keys) creating multiple entries for keys separated by regex splitter."
  [m splitter]
  (apply hash-map (flatten
    (map #(let [splitted (split (first %) splitter)]
            (if (> (count splitted) 1)
                (map vector splitted (repeat (second %)))
                %)) m))))

(def *translation-map*
  (split-keys {"drop" "drop-item"
               "get|take" "take-item"
               "i|inv" "inventory"
               "n|north" "move north"
               "w|west" "move west"
               "s|south" "move south"
               "e|east" "move east"
               "u|up" "move up"
               "d|down" "move down"
               "alias" "alias-command"
               "name" "name-room"} #"\|"))

(def *base-valid-actions* #{:drop-item :take-item :inventory :move :alias-command :name-room :dig :help :equip :look})

(defn valid-actions
  "returns all the callable actions from base ones, aliases and user-defined"
  [aliases]
  (into #{}
    (concat
      (map keyword (keys aliases))
      *base-valid-actions*)))

;; room = (x y z [on-the-ground])
(def *world-state* {:maze {[0 0 0] #{:sledge}
                           [1 1 5] #{:lots-of-gold}}
                    :inventory #{}
                    :current-room [0 0 0]
                    :equipped nil
                    :aliases *translation-map*
                    :named-rooms {[0 0 0] "the starting room"
                                  [1 1 5] "the prince room"}})

(def *directions* [:north :west :south :east :up :down])

(def *coords*
  (zipmap *directions*  [[0  1  0 ]
                         [-1 0  0 ]
                         [0 -1  0 ]
                         [1  0  0 ]
                         [0  0  1 ]
                         [0  0  -1]]))

(defn translate
  "if source is a key of target, returns the value as a sequence (split by spaces), else returns source."
  [source target]
  (split (if-let [t (get target source)] t source) #" "))

;; ** utilities ** ;;

(defn coord-at
  "given a coordinate (eg. [1 1 1]) and a direction (eg. :north) returns the coordinate for the direction (eg. [1 2 1])"
  [current direction]
  (vec (map + (direction *coords*) current)))

(defn current-room
  "returns the room the player is in, eg. [[1 2 3] #{:gold}]"
  [world]
  [(:current-room world) ((world :maze) (world :current-room ))])

(defn room-position
  "returns the coordinate for room (eg. [1 2 1])"
  [room]
  (first room))

(defn room-ground
  "returns the content of room (eg. #{gold})"
  [room]
  (second room))

(defn in?
  "returns true if k is in sequence"
  [coll k]
  (some #{k} coll))

(defn in-inv?
  "returns true if the player has item in her inventory"
  [world item]
  (in? (world :inventory) item))

(defn new-room-at
  "returns a new room relative to the player's current position, towards direction"
  [world direction]
  [(coord-at (world :current-room) direction) (hash-set (rand-nth [:gold :sledge :ladder]))])

(defn valid-direction
  "fn if direction is valid, otherwise prints an error message, else fnelse"
  [direction]
  (in? *directions* direction))

;; ** describing rooms ** ;;

(defn find-exits
  "returns a list of directions with the exit directions for room, eg. (:north :east)"
  [room world]
  (let [calc-neighbour #(map + (room-position room) (second %))
        maze (world :maze)]
    (->> *coords* (filter #(contains? maze (calc-neighbour %))) (map key))))

(defn exit?
  "returns true if room has an exit towards direction"
  [room world direction]
  (in? (find-exits room world) direction))

(defn describe-exits
  "returns string describing the exits in room"
  [room world]
  (let [exits-to-string (fn [exits]
          (cond
          (= (count exits) 1) (str " There is an exit " (name (first exits)) "ward")
          (not-empty exits) (str " There are exits at " (->> exits (map name) (join ", ")) ".")
          :else ""))]
    (exits-to-string (find-exits room world))))

(defn describe-items
  "returns a description of items"
  [items]
  (let [i (reduce #(conj %1
            (case %2
              :sledge "a sledge"
              :gold "some gold coins"
              :ladder "a ladder lying down"
              :lots-of-gold "LOTS of gold!"))
            [] items)]
     (str
       (join ", " (drop-last 2 i))
       (when (> (count i) 2) ", ")
       (join " and " (take-last 2 i)))))

(defn describe-ground
  "returns string describing the exits in room"
  [room]
  (let [room-content (room-ground room)]
  (if  (not-empty room-content)
     (str " On the ground you can see: " (describe-items room-content) "."))))

(defn describe
  "prints a description of room"
  [room world]
  (let [room-name (if-let [r ((world :named-rooms) (room-position room))]
          (str " (" r ")")
          nil)]
    (str "You are at "
      (join " "  (room-position room))
      room-name "."
      (describe-ground room)
      (describe-exits room world))))

(defn perform-action
  "executes action with args providing it with world state. if the function returns a string, prints it and return
 world. if function returns [string updated-world], prints string and return updated-world"
  [world action args]
  (let [result (apply (resolve (symbol action)) (conj args world ))]
    (cond
      (string? result) (do (println result) world)
      (vector? result) (do (println (first result)) (second result) ))))

;; ** actions ** ;;

(defn look [world]
  "describes the room the player is in"
  (describe (current-room world) world))

(defn dig
  "digs a new room towards direction if there's not a room already and the player has a sledge"
  ([world direction]
    (let [dir (keyword direction)]
      (if (valid-direction dir)
        (if (exit? (current-room world) world dir)
          "There is already a room!"
          (if (= (world :equipped) :sledge)
            [(str "You dig a new room " direction "ward.")
              (assoc world :maze (merge (world :maze) (new-room-at world dir)))]
            "You need to equip a sledge in order to dig the wall!"))
        "Where?!")))
  ([world] "Where do you want to dig?"))

(defn move
  "moves the player to an adjacent room and describes it"
  ([world direction]
    (let [dir (keyword direction)
          target-coord (coord-at (room-position (current-room world)) dir)]
      (if (valid-direction dir)
        (if (exit? (current-room world) world dir)
          (if (and (= dir :up) (not ((room-ground (current-room world)) :ladder)))
            "You cannot go up if there's no ladder in the room."
            (let [updated-world (assoc world :current-room target-coord)]
              [(describe (current-room updated-world) updated-world)
               updated-world]))
          "There's no exit in that direction!")
        "Where?!")))
  ([world] "Where do you want to go?"))

(defn equip
  "equips an item if specified and if the player has it in her inventory"
  ([world item]
    (let [i (keyword item)]
      (if (in-inv? world i)
        ["Equipped!"
         (assoc world :equipped i)])
        "You haven't such an item"))
  ([world] "What do you want to equip?"))

(defn drop-item
  "drops an item in the inventory or all of them leaving it in the room"
  ([world item]
    (let [i (keyword item)
          current-position (get world :current-room)
          current-ground (room-ground (current-room world))
          current-maze (world :maze)
          current-inventory (world :inventory)
          update-room (partial assoc current-maze current-position)
          equipped-item (world :equipped)]
      (if (= i :all)
        ["Everything dropped!"
         (assoc world
            :equipped nil
            :inventory #{}
            :maze (update-room (clojure.set/union current-ground current-inventory)))]
        (if (in-inv? world i)
          ["Item dropped!"
           (assoc world
             :equipped (if (= equipped-item i) nil equipped-item)
             :inventory (disj current-inventory i)
             :maze (update-room (conj current-ground i)))]
          "You haven't such an item"))))
  ([world] "What do you want to drop?"))

(defn take-item
  "picks up an item from the ground or all of them putting them into the inventory"
  ([world item]
    (let [i (keyword item)
          current-position (get world :current-room)
          current-ground (room-ground (current-room world))
          current-maze (:maze world)
          current-inventory (world :inventory)
          update-room (partial assoc current-maze current-position)
          equipped-item (world :equipped)]
      (if (= i :all)
        ["Everything taken!"
         (assoc world
           :inventory (clojure.set/union current-inventory current-ground)
           :maze (update-room #{}))]
        (if (in? current-ground i)
          ["Item taken!"
           (assoc world
             :inventory (conj current-inventory i)
             :maze (update-room (disj current-ground i)))]
          "There is not such item on the ground!"))))
  ([world] "What do you want to pick up?"))

(defn inventory
  "prints what the player is carrying"
  [world]
  (let [inv (world :inventory)]
    (if (empty? inv)
       "You are not carrying anything"
       (str "You are carrying: " (describe-items inv)))))

(defn equip
  ([world item]
    (let [i (keyword item)]
      (if (in-inv? world i)
        ["Item equipped." (assoc world :equipped i)]
        "You haven't such item")))
  ([world] "What do you want to equip?"))

(defn alias-command
  "aliases command to alias"
  ([world alias & commands]
    (let [command (join " " commands)
          current-aliases (world :aliases)]
      [(str "Alias created for the command " command) (assoc world :aliases (assoc current-aliases alias command))]))
  ([world] "Alias what?")
  ([world alias] (str "Alias '" alias "' to what?")))

(defn name-room
  "tags the current location with alias"
  ([world & alias]
    (let [a (join " " alias)
          current-named-room (world :named-rooms)
          current-location (room-position (current-room world))]
      ["Done!" (assoc world :named-rooms (assoc current-named-room current-location a))]))
  ([world]  "What name?"))

(defn help
  "prints an help message"
  [world]
  (str
    "Welcome to the dungeon!\n"
    "You need a sledge to dig rooms and ladders to go upwards.\n"
    "Valid commands are: directions (north, south...), dig, take, drop, equip, inventory and look.\n"
    "Additionally you can tag rooms with the 'name' command and alias commands with 'alias'.\n"
    "Have fun!\n"))

;; ** user input ** ;;

(defn sanitize-input
  "lowercases input, splits it into words and trims the tokens"
  [input]
  (remove empty? (-> input .toLowerCase .trim (split #" "))))

(defn parse-input
  "interprets user input. returns an updated world state if the game has to continue, nil if the user inputs 'exit'"
  [input world]
  (if (not-empty input)
    (let [sanitized-input (sanitize-input input)
          command (translate (first sanitized-input) (world :aliases))
          i (concat command (rest sanitized-input))
          [action & args] i
          current-valid-actions (world :aliases)]
      (cond
        (= (first i) "exit") nil
        (contains? (valid-actions current-valid-actions) (keyword action))
          (try (perform-action world action args)
          (catch IllegalArgumentException e (println "Invalid arguments") world))
        :else (do (println "What do you mean?") world)))
    (do
      (println "Hm?!")
      world)))

;; main loop
(defn run
  "the main game loop"
  []
  (do
    (println "Welcome to the dungeon!\nGrab the sledge and make your way to room 1,1,5 for a non-existant prize!")
    (loop [input (read-line)
           world *world-state*]
      (if-let [w (parse-input input world)]
              (recur (read-line) w)
              (println "See you next time!")))))

(run)

```

