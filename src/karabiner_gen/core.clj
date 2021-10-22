(ns karabiner-gen.core
  (:require [cheshire.core :refer [generate-string]])
  (:refer-clojure :exclude [pop]))


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
      [{:type "basic"
        :from {:key_code "h"
               :modifiers {:mandatory "left_control"}}
        :to [{:key_code "left_arrow"}]}
       {:type "basic"
        :from {:key_code "j"
               :modifiers {:mandatory "left_control"}}
        :to [{:key_code "down_arrow"}]}
       {:type "basic"
        :from {:key_code "k"
               :modifiers {:mandatory "left_control"}}
        :to [{:key_code "up_arrow"}]}
       {:type "basic"
        :from {:key_code "l"
               :modifiers {:mandatory "left_control"}}
        :to [{:key_code "right_arrow"}]}

       {:type :basic
        :from {:key_code "1"}
        :to_if_alone [{:key_code "1"}]
        :to_if_held_down [{:shell_command "'/Library/Application Support/org.pqrs/Karabiner-Elements/bin/karabiner_cli' --select-profile 'C'"}]
        :parameters {"basic.to_if_alone_timeout_milliseconds" 250,
                     "basic.to_if_held_down_threshold_milliseconds" 250}}]}]}})


(defn layer-factory
  "Creates a function which is capable of defining manipulators that inherit the conditions from
  the layer factory."
  [conditions]
  (let [conditions (for [[cond name val] conditions]
                     {:type (if (= cond :if) "variable_if" "variable_unless")
                      :name name
                      :value val})]
    (fn layer-factory'
      ([from] (layer-factory' from from nil))
      ([from to] (layer-factory' from to nil))
      ([from to to-modifiers] (layer-factory' from nil to to-modifiers))
      ([from from-modifiers to to-modifiers]
       (let [from-modifiers (cond
                              (nil? from-modifiers) []
                              (coll? from-modifiers) from-modifiers
                              :else [from-modifiers])
             to-modifiers (cond
                            (nil? to-modifiers) []
                            (coll? to-modifiers) to-modifiers
                            :else [to-modifiers])]
         {:type "basic"
          :conditions conditions
          :from {:key_code from
                 :modifiers {:optional from-modifiers}}
          :to [{:key_code to
                :modifiers to-modifiers}]})))))


(def snap (layer-factory [[:if "snap" 1] [:if "snap-d" 0] [:if "snap-f" 0] [:if "pop" 0]]))
(def snap-d (layer-factory [[:if "snap-d" 1] [:if "pop" 0]]))
(def snap-f (layer-factory [[:if "snap-f" 1] [:if "pop" 0]]))
(def pop (layer-factory [[:if "snap" 0] [:if "pop" 1]]))
(def standard (layer-factory [[:if "snap" 0] [:if "pop" 0]]))


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
  [
   {:type :basic
    :conditions [{:type "variable_if" :name "lang" :value 1}]
    :from {:key_code from}
    :to [{:key_code "e" :modifiers ["option"]}
         {:key_code from}]
    :to_after_key_up [{:set_variable {:name "lang" :value 0}}]}

   {:type :basic
    :conditions [{:type "variable_if" :name "lang" :value 2}]
    :from {:key_code from}
    :to [{:key_code "j" :modifiers ["option"]}
         {:key_code from}]
    :to_after_key_up [{:set_variable {:name "lang" :value 0}}]}

   {:type :basic
    :conditions [{:type "variable_if" :name "lang" :value 3}]
    :from {:key_code from}
    :to [{:key_code "u" :modifiers ["option"]}
         {:key_code from}]
    :to_after_key_up [{:set_variable {:name "lang" :value 0}}]}

   ; Capital letters
   {:type :basic
    :conditions [{:type "variable_if" :name "lang" :value 1}]
    :from {:key_code from
           :modifiers {:mandatory ["shift"]}}
    :to [{:key_code "e" :modifiers ["option"]}
         {:key_code from :modifiers ["shift"]}]
    :to_after_key_up [{:set_variable {:name "lang" :value 0}}]}

   {:type :basic
    :conditions [{:type "variable_if" :name "lang" :value 2}]
    :from {:key_code from
           :modifiers {:mandatory ["shift"]}}
    :to [{:key_code "j" :modifiers ["option"]}
         {:key_code from :modifiers ["shift"]}]
    :to_after_key_up [{:set_variable {:name "lang" :value 0}}]}

   {:type :basic
    :conditions [{:type "variable_if" :name "lang" :value 3}]
    :from {:key_code from
           :modifiers {:mandatory ["shift"]}}
    :to [{:key_code "u" :modifiers ["option"]}
         {:key_code from :modifiers ["shift"]}]
    :to_after_key_up [{:set_variable {:name "lang" :value 0}}]}
   ])


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
        [
         ; df       -> lang1  (Option-e) ó
         ; lang1 d  -> lang3  (Option-u) ö
         ; lang1 f  -> lang2  (Option-j) ő

         {:type :basic
          :conditions [{:type "variable_if" :name "lang" :value 0}]
          :from {:simultaneous [{:key_code "d"}
                                {:key_code "f"}]
                 :modifiers {:optional ["any"]}}
          :to [{:set_variable {:name "lang" :value 1}}]}

         {:type :basic
          :conditions [{:type "variable_if" :name "lang" :value 1}]
          :from {:simultaneous [{:key_code "f"}]
                 :modifiers {:optional ["any"]}}
          :to [{:set_variable {:name "lang" :value 2}}]}

         {:type :basic
          :conditions [{:type "variable_if" :name "lang" :value 1}]
          :from {:simultaneous [{:key_code "d"}]
                 :modifiers {:optional ["any"]}}
          :to [{:set_variable {:name "lang" :value 3}}]}


         (lang "a")
         (lang "e")
         (lang "i")
         (lang "o")
         (lang "u")


         ; snap on
         {:type "basic"
          :conditions [{:type "variable_if" :name "snap" :value 0}
                       {:type "variable_if" :name "pop" :value 0}]
          :from {:key_code "caps_lock"
                 :modifiers {:optional ["any"]}}
          :to [{:set_variable {:name "snap" :value 1}}]
          :to_after_key_up [{:set_variable {:name "snap" :value 0}}]}
         {:type "basic"
          :conditions [{:type "variable_if" :name "snap" :value 0}
                       {:type "variable_if" :name "pop" :value 0}]
          :from {:key_code "quote"
                 :modifiers {:optional ["any"]}}
          :to [{:set_variable {:name "snap" :value 1}}]
          :to_after_key_up [{:set_variable {:name "snap" :value 0}}]}
         ; pop on
         {:type "basic"
          :conditions [{:type "variable_if" :name "snap" :value 0}
                       {:type "variable_if" :name "pop" :value 0}]
          :from {:key_code "open_bracket"
                 :modifiers {:optional ["any"]}}
          :to [{:set_variable {:name "pop" :value 1}}]
          :to_after_key_up [{:set_variable {:name "pop" :value 0}}]}

         ; SNAP-D
         {:type "basic"
          :conditions [{:type "variable_if" :name "snap" :value 1}
                       {:type "variable_if" :name "snap-f" :value 0}
                       {:type "variable_if" :name "pop" :value 0}]
          :from {:key_code "d"
                 :modifiers {:optional ["any"]}}
          :to [{:set_variable {:name "snap-d" :value 1}}]
          :to_if_alone [{:key_code "hyphen"}]
          :to_after_key_up [{:set_variable {:name "snap-d" :value 0}}]}

         ; SNAP-F
         {:type "basic"
          :conditions [{:type "variable_if" :name "snap" :value 1}
                       {:type "variable_if" :name "snap-d" :value 0}
                       {:type "variable_if" :name "pop" :value 0}]
          :from {:key_code "f"
                 :modifiers {:optional ["any"]}}
          :to [{:set_variable {:name "snap-f" :value 1}}]
          :to_if_alone [{:key_code "quote" :modifiers "shift"}]
          :to_after_key_up [{:set_variable {:name "snap-f" :value 0}}]}

         ; changes in standard mode
         (standard "tab" "left_control")
         (standard "semicolon" "return_or_enter")
         (standard "comma" "delete_or_backspace")
         ; (standard "slash" "escape")

         ; this control can be used in compound shortcuts like
         ; Command-Control-Shift-4 (make screenshot into clipboard)
         (standard "period" ["any"] "left_control" [])

         ; to change between windows of a program comfortable on a hungarian keyboard
         (standard "non_us_backslash" "command" "grave_accent_and_tilde" "command")

         ; snap layer
         ; top row
         (snap "tab" "hyphen" "shift")                      ; -> _
         (snap "q" "comma" ["shift"])                       ; -> <
         (snap "w" "period" ["shift"])                      ; -> >
         (snap "e" "9" "shift")                             ; -> (
         (snap "r" "0" "shift")                             ; -> )
         (snap "t" "slash")                                 ; -> /
         (snap "y" "backslash")                             ; -> /
         (snap "u" "open_bracket" "shift")                  ; -> {
         (snap "i" "close_bracket" "shift")                 ; -> }
         (snap "o" "open_bracket")                          ; -> [
         (snap "p" "close_bracket")                         ; -> ]


         (snap "open_bracket" "backslash" "shift")
         (snap "close_bracket" "grave_accent_and_tilde")

         ; middle row
         (snap "quote" "right_arrow" ["command"])
         (snap "semicolon" "left_arrow" ["command"])
         (snap "l" "right_arrow")
         (snap "k" "up_arrow")
         (snap "j" "down_arrow")
         (snap "h" "left_arrow")
         (snap "g" "quote")
         ;(snap "f" "quote" "shift")
         ;(snap "d" "hyphen")
         (snap "s" "equal_sign" "shift")
         (snap "a" "equal_sign")
         (snap "caps_lock" "semicolon" "shift")

         ; bottom row
         (snap "right_shift" "escape")
         (snap "slash" "period" "shift")
         (snap "period" "period")
         (snap "comma" "comma")
         (snap "m" "page_up")
         (snap "n" "page_down")
         (snap "b" "grave_accent_and_tilde" "shift")
         ; Free spots:
         ;(snap "v" "9" "shift")
         ;(snap "c" "open_bracket")
         ;(snap "x" "open_bracket" "shift")
         ;(snap "z" "comma" "shift")
         (snap "left_shift" "tab")

         (snap-d "h" "h" ["command" "option" "control" "shift"]) ; load file in repl
         (snap-d "j" "j" ["command" "option" "control" "shift"]) ; eval top form
         (snap-d "k" "k" ["command" "option" "control" "shift"]) ; clear repl
         (snap-d "l" "l" ["command" "option" "control" "shift"]) ; slurp forward
         (snap-d "p" "p" ["command" "option" "control" "shift"]) ; run tests in current ns
         (snap-d "o" "o" ["command" "option" "control" "shift"]) ; run test under caret
         (snap-d "i" "i" ["command" "option" "control" "shift"]) ; raise sexp
         (snap-d "m" "m" ["command" "option" "control" "shift"]) ; splice sexp
         (snap-d "n" "n" ["command" "option" "control" "shift"]) ; recent files
         (snap-d "b" "b" ["command" "option" "control" "shift"]) ; restart system
         (snap-d "u" "u" ["command" "option" "control" "shift"]) ; rerun last test action


         ; selection, copy-paste, undo, redo
         (snap-f "quote" "right_arrow" ["command" "shift"])
         (snap-f "semicolon" "left_arrow" ["command" "shift"])
         (snap-f "l" "right_arrow" "shift")
         (snap-f "k" "up_arrow" "shift")
         (snap-f "j" "down_arrow" "shift")
         (snap-f "h" "left_arrow" "shift")
         (snap-f "i" "i" ["option" "control" "shift"])
         (snap-f "b" "v" "command")                         ; paste
         (snap-f "n" "c" "command")                         ; copy
         (snap-f "m" "x" "command")                         ; cut
         (snap-f "u" "z" "command")                         ; undo
         (snap-f "y" "z" ["shift" "command"])               ; redo
         (snap-f "o" "o" ["option" "control" "shift"])      ; redo

         ; pop
         (pop "d" "open_bracket" ["command" "shift"])       ; select previous tab
         (pop "f" "close_bracket" ["command" "shift"])      ; select next tab
         (pop "j" "open_bracket" ["command"])               ; select next tab
         (pop "k" "close_bracket" ["command"])              ; select next tab
         (pop "q" "1" ["shift"])                            ; !
         (pop "w" "2" ["shift"])                            ; @
         (pop "e" "3" ["shift"])                            ; #
         (pop "r" "4" ["shift"])                            ; $
         (pop "t" "5" ["shift"])                            ; %
         (pop "y" "6" ["shift"])                            ; ^
         (pop "u" "7" ["shift"])                            ; &
         (pop "i" "8" ["shift"])                            ; *
         (pop "c" "close_bracket")                          ; ]
         (pop "v" "0" ["shift"])                            ; )
         (pop "x" "close_bracket" ["shift"])                ; }
         (pop "a" "slash" ["shift"])
         (pop "s" "semicolon")

         ;
         {:type :basic
          :from {:key_code "1"}
          :to_if_alone [{:key_code "1"}]
          :to_if_held_down [{:shell_command "'/Library/Application Support/org.pqrs/Karabiner-Elements/bin/karabiner_cli' --select-profile 'N'"}]
          :parameters {"basic.to_if_alone_timeout_milliseconds" 250,
                       "basic.to_if_held_down_threshold_milliseconds" 250}}

         ;; if app is Anki
         ;; left command -> 1 (don't know)
         ;; space
         ])}]}})

(comment
  (spit "/Users/david/.config/karabiner/karabiner.json"
    (generate-string
      (-> config
        (update :profiles conj capsvim-profile)
        (update :profiles conj crackle-profile))
      {:pretty true})))
