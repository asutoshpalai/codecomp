(asdf:defsystem lem-codecomp
  :description "Lem LLM code suggestion plugin"
  :version "0.1"
  :author "Asutosh Palai"
  :license "MIT"
  :depends-on (:dexador :alexandria :yason)
  :components ((:file "codecomp" :depends-on ("langchain"))
               (:file "langchain")))