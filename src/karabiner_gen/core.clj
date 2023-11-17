(ns karabiner-gen.core
  (:require [cheshire.core :refer [generate-string]])
  (:refer-clojure :exclude [pop]))


(def bundle {:intellij "com.jetbrains.intellij"
             :sublime  "com.sublimetext.4"
             :vscode   "com.microsoft.VSCode"})


(def config
  {:global {"check_for_updates_on_startup" true
            "show_in_menu_bar" true
            "show_profile_name_in_menu_bar" true}
   :profiles []})


(def capsvim-profile
  {:name "N"

   :virtual_hid_keyboard
   {:caps_lock_delay_milliseconds 0
    :keyboard_type "ansi"}

   :simple_modifications
   [{:from {:key_code "caps_lock"}
     :to {:key_code "left_control"}}]

   :complex_modifications
   {:parameters {}
    :rules
    [{:description "bla bla"
      :manipulators
      [{:type :basic
        :from {:key_code "h"
               :modifiers {:mandatory "left_control"}}
        :to [{:key_code "left_arrow"}]}
       {:type :basic
        :from {:key_code "j"
               :modifiers {:mandatory "left_control"}}
        :to [{:key_code "down_arrow"}]}
       {:type :basic
        :from {:key_code "k"
               :modifiers {:mandatory "left_control"}}
        :to [{:key_code "up_arrow"}]}
       {:type :basic
        :from {:key_code "l"
               :modifiers {:mandatory "left_control"}}
        :to [{:key_code "right_arrow"}]}

       {:type :basic
        :from {:key_code "1"}
        :to_if_alone [{:key_code "1"}]
        :to_if_held_down [{:shell_command "'/Library/Application Support/org.pqrs/Karabiner-Elements/bin/karabiner_cli' --select-profile 'C'"}]
        :parameters {"basic.to_if_alone_timeout_milliseconds" 250,
                     "basic.to_if_held_down_threshold_milliseconds" 250}}]}]}})


(defn basic-modification
  ([from] (basic-modification from from nil))
  ([from to] (basic-modification from to nil))
  ([from to to-modifiers] (basic-modification from nil to to-modifiers))
  ([from from-modifiers to to-modifiers]
   (let [from-modifiers (cond
                          (nil? from-modifiers) []
                          (coll? from-modifiers) from-modifiers
                          :else [from-modifiers])
         to-modifiers (cond
                        (nil? to-modifiers) []
                        (coll? to-modifiers) to-modifiers
                        :else [to-modifiers])]
     {:type :basic
      :conditions []
      :from {:key_code from
             :modifiers {:mandatory from-modifiers}}
      :to [{:key_code to
            :modifiers to-modifiers}]})))


(defn add-var-if-condition
  [next-fn name value]
  (fn [& args]
    (let [result (apply next-fn args)]
      (println result)
      (update result :conditions conj {:type :variable_if :name name :value value}))))


(defn add-var-if-conditions
  [next-fn condition-map]
  (fn [& args]
    (let [result (apply next-fn args)]
      (reduce-kv
       (fn [result k v]
         (update result :conditions conj {:type :variable_if :name k :value v}))
       result
       condition-map))))


(defn add-app-prefix-condition
  [next-fn]
  (fn [& args]
    (let [bundle (get bundle (first args) (first args))
          result (apply next-fn (rest args))]
      (update result :conditions conj {:type "frontmost_application_if" :bundle_identifiers [bundle]}))))


(defn add-description
  [next-fn]
  (fn [& args]
    (let [last-param (last args)]
      (if (string? last-param)
        (let [result (apply next-fn (butlast args))]
          (assoc result :description last-param))
        (apply next-fn args)))))


(def standard (add-var-if-conditions basic-modification {:snap 0 :snap-d 0 :snap-f 0 :pop 0 :slash 0}))
(def snap (add-var-if-conditions basic-modification {:snap 1 :snap-d 0 :snap-f 0 :pop 0 :slash 0}))
(def snap-d (add-var-if-conditions basic-modification {:snap 1 :snap-d 1 :snap-f 0 :pop 0 :slash 0}))
(def snap-f (add-var-if-conditions basic-modification {:snap 1 :snap-d 0 :snap-f 1 :pop 0 :slash 0}))
(def pop (add-var-if-conditions basic-modification {:snap 0 :snap-d 0 :snap-f 0 :pop 1 :slash 0}))
(def slash (add-var-if-conditions basic-modification {:snap 0 :snap-d 0 :snap-f 0 :pop 0 :slash 1}))


(def app-snap     (-> basic-modification
                      (add-description)
                      (add-var-if-conditions {:snap 1 :snap-d 0 :snap-f 0 :pop 0 :slash 0})
                      (add-app-prefix-condition)))
(def app-snap-f   (-> basic-modification
                      (add-description)
                      (add-var-if-conditions {:snap 1 :snap-d 0 :snap-f 1 :pop 0 :slash 0})
                      (add-app-prefix-condition)))
(def app-snap-d   (-> basic-modification
                      (add-description)
                      (add-var-if-conditions {:snap 1 :snap-d 1 :snap-f 0 :pop 0 :slash 0})
                      (add-app-prefix-condition)))
(def app-standard (-> basic-modification
                      (add-description)
                      (add-var-if-conditions {:snap 0 :snap-d 0 :snap-f 0 :pop 0 :slash 0})
                      (add-app-prefix-condition)))


(defn lang
  "Here I take advantage of fact with sticky modifiers it is possible to insert accents.

  Extended ABC keyboard
  ---------------------
  a          -> a
  Option-e a -> á
  Option-u a -> ä

  e          -> e
  Option-e e -> é

  i          -> i
  Option-e i -> í

  o          -> o
  Option-e o -> ó
  Option-u o -> ö
  Option-j o -> ő

  u          -> u
  Option-e u -> ú
  Option-u u -> ü
  Option-j u -> ű"
  [from]
  [{:type :basic
    :conditions [{:type :variable_if :name :lang :value 1}]
    :from {:key_code from}
    :to [{:key_code "e" :modifiers [:option]}
         {:key_code from}]
    :to_after_key_up [{:set_variable {:name :lang :value 0}}]}

   {:type :basic
    :conditions [{:type :variable_if :name :lang :value 2}]
    :from {:key_code from}
    :to [{:key_code "j" :modifiers [:option]}
         {:key_code from}]
    :to_after_key_up [{:set_variable {:name :lang :value 0}}]}

   {:type :basic
    :conditions [{:type :variable_if :name :lang :value 3}]
    :from {:key_code from}
    :to [{:key_code "u" :modifiers [:option]}
         {:key_code from}]
    :to_after_key_up [{:set_variable {:name :lang :value 0}}]}

   ; Capital letters
   {:type :basic
    :conditions [{:type :variable_if :name :lang :value 1}]
    :from {:key_code from
           :modifiers {:mandatory [:shift]}}
    :to [{:key_code "e" :modifiers [:option]}
         {:key_code from :modifiers [:shift]}]
    :to_after_key_up [{:set_variable {:name :lang :value 0}}]}

   {:type :basic
    :conditions [{:type :variable_if :name :lang :value 2}]
    :from {:key_code from
           :modifiers {:mandatory [:shift]}}
    :to [{:key_code "j" :modifiers [:option]}
         {:key_code from :modifiers [:shift]}]
    :to_after_key_up [{:set_variable {:name :lang :value 0}}]}

   {:type :basic
    :conditions [{:type :variable_if :name :lang :value 3}]
    :from {:key_code from
           :modifiers {:mandatory [:shift]}}
    :to [{:key_code "u" :modifiers [:option]}
         {:key_code from :modifiers [:shift]}]
    :to_after_key_up [{:set_variable {:name :lang :value 0}}]}])


(defn cond-app-if
  [app]
  {:type :frontmost_application_if
   :bundle_identifiers [app]})


(def crackle-profile
  {:name "C"
   :selected true
   :virtual_hid_keyboard
   {:caps_lock_delay_milliseconds 0
    :keyboard_type "ansi"}
   :complex_modifications
   {:parameters {}
    :rules
    [{:description "bla bla"
      :manipulators
      (flatten
       [;; Language modes:
         ;;
         ;;  df       -> lang1  (Option-e) ó
         ;;  lang1 d  -> lang3  (Option-u) ö
         ;;  lang1 f  -> lang2  (Option-j) ő

        {:type :basic
         :conditions [{:type :variable_if :name :lang :value 0}]
         :from {:simultaneous [{:key_code :d}
                               {:key_code :f}]
                :modifiers {:optional [:any]}}
         :to [{:set_variable {:name :lang :value 1}}]}

        {:type :basic
         :conditions [{:type :variable_if :name :lang :value 1}]
         :from {:simultaneous [{:key_code :f}]
                :modifiers {:optional [:any]}}
         :to [{:set_variable {:name :lang :value 2}}]}

        {:type :basic
         :conditions [{:type :variable_if :name :lang :value 1}]
         :from {:simultaneous [{:key_code :d}]
                :modifiers {:optional [:any]}}
         :to [{:set_variable {:name :lang :value 3}}]}


        (lang :a)
        (lang :e)
        (lang :i)
        (lang :o)
        (lang :u)


         ;; ======================================================
         ;; Layer activation shortcuts

         ;; SNAP
        {:type :basic
         :conditions [{:type :variable_if :name :snap :value 0}
                      {:type :variable_if :name :pop :value 0}
                      {:type :variable_if :name :slash :value 0}]
         :from {:key_code :caps_lock
                :modifiers {:optional [:any]}}
         :to [{:set_variable {:name :snap :value 1}}]
         :to_after_key_up [{:set_variable {:name :snap :value 0}}]}
        {:type :basic
         :conditions [{:type :variable_if :name :snap :value 0}
                      {:type :variable_if :name :pop :value 0}
                      {:type :variable_if :name :slash :value 0}]
         :from {:key_code :quote
                :modifiers {:optional [:any]}}
         :to [{:set_variable {:name :snap :value 1}}]
         :to_after_key_up [{:set_variable {:name :snap :value 0}}]}

         ;; POP
        {:type :basic
         :conditions [{:type :variable_if :name :snap :value 0}
                      {:type :variable_if :name :pop :value 0}
                      {:type :variable_if :name :slash :value 0}]
         :from {:key_code :open_bracket
                :modifiers {:optional [:any]}}
         :to [{:set_variable {:name :pop :value 1}}]
         :to_after_key_up [{:set_variable {:name :pop :value 0}}]}

         ; SNAP-D
        {:type :basic
         :conditions [{:type :variable_if :name :snap :value 1}
                      {:type :variable_if :name :snap-f :value 0}
                      {:type :variable_if :name :pop :value 0}
                      {:type :variable_if :name :slash :value 0}]
         :from {:key_code :d
                :modifiers {:optional [:any]}}
         :to [{:set_variable {:name :snap-d :value 1}}]
         :to_if_alone [{:key_code :hyphen}]
         :to_after_key_up [{:set_variable {:name :snap-d :value 0}}]}

         ; SNAP-F
        {:type :basic
         :conditions [{:type :variable_if :name :snap :value 1}
                      {:type :variable_if :name :snap-d :value 0}
                      {:type :variable_if :name :pop :value 0}
                      {:type :variable_if :name :slash :value 0}]
         :from {:key_code :f
                :modifiers {:optional [:any]}}
         :to [{:set_variable {:name :snap-f :value 1}}]
         :to_if_alone [{:key_code :quote :modifiers :shift}]
         :to_after_key_up [{:set_variable {:name :snap-f :value 0}}]}

         ; SLASH
        {:type :basic
         :conditions [{:type :variable_if :name :snap :value 0}
                      {:type :variable_if :name :pop :value 0}]
         :from {:key_code :slash
                :modifiers {:optional [:any]}}
         :to [{:set_variable {:name :slash :value 1}}]
         :to_after_key_up [{:set_variable {:name :slash :value 0}}]}


         ;; =====================================================================
         ;; SNAP top row
        (standard :tab :left_control)
        (standard :semicolon :return_or_enter)
        (standard :comma :delete_or_backspace)
        (standard :period [:any] :left_control [])
         ; to change between windows of a program comfortable on a hungarian keyboard
        (standard :non_us_backslash :command :grave_accent_and_tilde :command)


         ;; =====================================================================
         ;; SNAP top row
        (snap :tab :hyphen :shift)                                             ; _
         ;; snap q : (app specific) delete line
         ;; snap w : (app specific) join lines
         ;; snap e : (app specific) duplicate lines
         ;; snap r : (app specific) comment line
        (snap :t :slash)                                                       ; /
        (snap :y :backslash)                                                   ; \
        (snap :u :left_arrow :option)
        (snap :i :right_arrow :option)
         ;; snap o : FREE
         ;; snap p : FREE
        (snap :open_bracket :backslash :shift)                                 ; |
        (snap :close_bracket :grave_accent_and_tilde)                          ; `


         ;; =====================================================================
         ;; SNAP middle row
        (snap :quote :right_arrow [:command])
        (snap :semicolon :left_arrow [:command])
        (snap :l :right_arrow)
        (snap :k :up_arrow)
        (snap :j :down_arrow)
        (snap :h :left_arrow)
        (snap :g :quote)
        (snap :s :equal_sign :shift)
        (snap :a :equal_sign)
        (snap :caps_lock :semicolon :shift)


         ;; =====================================================================
         ;; SNAP bottom row

        (snap :right_shift :escape)
        (snap :slash :period :shift)
        (snap :period :period)
        (snap :comma :comma)
        (snap :m :page_up)
        (snap :n :page_down)
        (snap :b :grave_accent_and_tilde :shift)
        (snap :left_shift :tab)


        (snap-f :quote :right_arrow [:command :shift])                         ; select until end of line
        (snap-f :semicolon :left_arrow [:command :shift])                      ; select until beginning of line
        (snap-f :l :right_arrow :shift)                                        ; select forward
        (snap-f :k :up_arrow :shift)                                           ; select upward
        (snap-f :j :down_arrow :shift)                                         ; select downward
        (snap-f :h :left_arrow :shift)                                         ; select backward
        (snap-f :u :left_arrow [:shift :option])                               ; select word backward
        (snap-f :i :right_arrow [:shift :option])                              ; select word forward
        (snap-f :b :v :command)                                                ; paste
        (snap-f :n :c :command)                                                ; copy
        (snap-f :m :x :command)                                                ; cut


        (pop :d :open_bracket [:command :shift])                               ; select previous tab
        (pop :f :close_bracket [:command :shift])                              ; select f tab
        (pop :j :open_bracket [:command])                                      ; select next tab
        (pop :k :close_bracket [:command])                                     ; select next tab
        (pop :q :1 [:shift])                                                   ; !
        (pop :w :2 [:shift])                                                   ; @
        (pop :e :3 [:shift])                                                   ; #
        (pop :r :4 [:shift])                                                   ; $
        (pop :t :5 [:shift])                                                   ; %
        (pop :y :6 [:shift])                                                   ; ^
        (pop :u :7 [:shift])                                                   ; &
        (pop :i :8 [:shift])                                                   ; *
        (pop :c :close_bracket)                                                ; ]
        (pop :v :0 [:shift])                                                   ; )
        (pop :x :close_bracket [:shift])                                       ; }
        (pop :a :slash [:shift])
        (pop :s :semicolon)


        (slash :a :open_bracket)                                               ; -> [
        (slash :s :close_bracket)                                              ; -> ]
        (slash :d :open_bracket :shift)                                        ; -> {
        (slash :f :close_bracket :shift)                                       ; -> {
        (slash :q :comma :shift)                                               ; -> <
        (slash :w :period :shift)                                              ; -> >
        (slash :e :9 :shift)                                                   ; -> (
        (slash :r :0 :shift)                                                   ; -> )

         ;
        {:type :basic
         :from {:key_code "1"}
         :to_if_alone [{:key_code "1"}]
         :to_if_held_down [{:shell_command "'/Library/Application Support/org.pqrs/Karabiner-Elements/bin/karabiner_cli' --select-profile 'N'"}]
         :parameters {"basic.to_if_alone_timeout_milliseconds" 250,
                      "basic.to_if_held_down_threshold_milliseconds" 250}}

         ;; ==============================================================================
         ;; App specific commands
         ;; snap-f o : select line
         ;; snap-f p : add next occurence to selection
         ;; snap-f y : column selection
         ;; snap f , : expand selection
         ;; snap q   : delete line
         ;; snap w   : join lines
         ;; snap e   : duplicate line
         ;; snap r   : comment line
         ;; F3       : quick find

         ;; Move line / selection up
         ;; Move lien / selection down


         ;; Intellij
        (app-snap-f :intellij :o :w [:command :option :control :shift])        ; custom intellij shortcut: Extend Line Selection
        (app-snap-f :intellij :p :g [:command :option :control :shift])        ; custom intellij shortcut: Add Selection for Next Occurance
        (app-snap-f :intellij :y :8 [:command :shift])
        (app-snap-f :intellij :comma :up_arrow :option)
        (app-snap :intellij :q :delete_or_backspace :command)
        (app-snap :intellij :w :j [:control :shift])
        (app-snap :intellij :e :d :command)
        (app-snap :intellij :r :slash :command)
        (app-standard :intellij :f3 :g [:option])                              ; requires intellij plugin: https://plugins.jetbrains.com/plugin/10635-quick-find/versions

         ;; Sublime
        (app-snap-f :sublime :o :l [:command])
        (app-snap-f :sublime :p :d [:command])
        (app-snap-f :sublime :y :down_arrow [:control :shift])
        (app-snap-f :sublime :comma :q [:control :shift :option])              ; custom sublime shortcut: expand region (https://github.com/aronwoost/sublime-expand-region)
        (app-snap :sublime :q :k [:control :shift])
        (app-snap :sublime :w :j [:command :shift])
        (app-snap :sublime :e :d [:command :shift])
        (app-snap :sublime :r :slash [:command])
        (app-standard :sublime :f3 :g [:command :option])

         ;; VS Code
        (app-snap-f   :vscode   :o                :l           [:command]                 "select line")
        (app-snap-f   :vscode   :p                :d           [:command]                 "add next occurance to selection")
        (app-snap-f   :vscode   :y                :down_arrow  [:command :option]         "column selectio down")
        (app-snap-f   :vscode   :comma            :right_arrow [:control :shift :command] "expand selection")
        (app-snap     :vscode   :q                :k           [:command :shift]          "delete line")
        (app-snap     :vscode   :w                :j           [:control]                 "join lines")
        (app-snap     :vscode   :e                :down_arrow  [:shift :option]           "dupliacte line")
        (app-snap     :vscode   :r                :slash       [:command]                 "comment out line")
        (app-standard :vscode   :g     [:control]   :period      [:control :shift :command] "quick fix")
         ;; quick find to F3: othing to do F3 is already bound to quick find in VS Code


         ;; ===========================================================================
         ;; App speficic special commands (commands that make sense in this particular application)

        (app-snap-d :intellij :h :h [:command :option :control :shift])        ; load file in repl
        (app-snap-d :intellij :j :j [:command :option :control :shift])        ; eval top form
        (app-snap-d :intellij :k :k [:command :option :control :shift])        ; clear repl
        (app-snap-d :intellij :l :l [:command :option :control :shift])        ; slurp forward
        (app-snap-d :intellij :p :p [:command :option :control :shift])        ; run tests in current ns
        (app-snap-d :intellij :o :o [:command :option :control :shift])        ; run test under caret
        (app-snap-d :intellij :i :i [:command :option :control :shift])        ; raise sexp
        (app-snap-d :intellij :m :m [:command :option :control :shift])        ; splice sexp
        (app-snap-d :intellij :n :n [:command :option :control :shift])        ; recent files
        (app-snap-d :intellij :b :b [:command :option :control :shift])        ; restart system
        (app-snap-d :intellij :u :u [:command :option :control :shift])        ; rerun last test action

        (app-snap-d :sublime :h :h [:control :shift :option]         "sublime: eval file")
        (app-snap-d :sublime :k :k [:control :shift :option]         "sublime: clear output")
        (app-snap-d :sublime :j :j [:control :shift :option]         "sublime: eval topmost form")

        (app-snap-d :vscode :j :j [:command :option :control :shift] "calva.evaluateSelection")
        (app-snap-d :vscode :h :h [:command :option :control :shift] "calva.loadFile")])}]}})


(defn write-out-config [file-path]
  (spit file-path
        (generate-string
         (-> config
             (update :profiles conj capsvim-profile)
             (update :profiles conj crackle-profile))
         {:pretty true}))
  (println "Config written to " file-path))

(write-out-config "/Users/david/.config/karabiner/karabiner.json")