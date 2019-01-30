#lang at-exp slideshow

(require
  slideshow/text
  slideshow/code)

(current-main-font "Source Serif Pro")

(define (pretty-table columns elements)
  (table
    (length columns)
    (append (map t columns)
            (map t elements))
    lc-superimpose
    cc-superimpose
    30
    10))

(define (code-example . txt)
  (codeblock-pict (apply string-append txt)))

(slide #:title "Why JSON when you can DSL?"
  @para{FOSDEM 2019}
  @para{Minimalistic Languages Developer Room}
  @para{Creating file formats & languages that fit your needs}
  @para{by @bt{Jérôme Martin}}
  @para{Developer at OVH (web/hosting/datacenters)}
  @para{Former developer at Ubisoft (video games)})

(slide #:title "What we expect from JSON"
  @para{JSON is used to represent structured data}
  @code-example|{
{
  "name": "My pony ranch",
  "owner": {
    "name": "Jerome",
    "surname": "Martin",
  },
  "ponies": [
    { "name": "Rarity", "level": "3" },
    { "name": "Applejack", "level": "2" },
    { "name": "Twilight", "level": "2" },
  ]
}
}|)

(slide #:title "How JSON betrays us"
  @para{It allows some types only: strings, numbers, arrays, dicts... that's it.}
  'next
  @para{It fails at representing processes and computations.}
  'next
  @para{It fails at self-referencing.})

(slide #:title "How JSON betrays us"
  @para{JSON is not enough, what do we do?}
  'next
  @para{We add javascript to it!}
  @para{<insert code here>}
  'next
  @para{Configuration files soon become monsters.}
  'next
  @para{WHY?})

(slide #:title "How JSON betrays us"
  @para{"A data structure is just a stupid programming language" -- Bill Gosper}
  'next
  @para{JSON, XML, HTML, CSS... are all stupid programming languages.}
  'next
  @para{When we try representing concepts with them, abstraction inherently leaks.}
  @para{We always end up writing the missing abstraction layer by hand.})

(slide #:title "In need for meta"
  @para{We try to make them less stupid:}
  @item{CSS → Less}
  @item{Javascript → Babel}
  @item{HTML → Mustache templates / JSX syntax}
  @item{C → Preprocessor}
  @item{C++ → Templates}
  @item{Python → meta-classes}
  @item{C# → Reflection})

(slide @para{There's a simpler way.})

(slide #:title "The DSL way"
  @para{Domain Specific Languages (DSL)}
  'next
  @para{The abstraction becomes a language.}
  'next
  @para{Examples: Makefile, GameMaker Language, Regexps, SQL, Qt...})

(slide #:title "The DSL way"
  @para{Structured data will always be better expressed with a specific language for that domain than a generic data structure.}
  'next
  @item{Banking data → banking language}
  @item{Game data → game language}
  @item{Medical data → medical language})

(slide @para{But isn't writing a full language excessive?})

(slide #:title "Rule #1: Abstraction leaks"
  @para{Any data eventually becomes a DSL *naturally* by leaking through abstractions.}
  'next
  @para{Most programs are tools made to prevent this leakage.}
  'next
  @para{Only most of the time they become a badly written half implementation of lisp.}
  'next
  @para{So why not using lisp in the first place?})

(slide #:title "Racket"
  @para{Racket, the language-oriented programming language}
  'next
  @para{As a lisp language, it allows writing itself *by design*.}
  @para{Racket is specialized in writing Domain Specific Languages (DSL)})

(slide #:title "How to make a DSL"
  @para{It takes 5 lines of Racket to implement a generic parser for any language.}
  @para{<insert code here>})

(slide #:title "How to make a DSL"
  @para{Using the lisp syntax called "s-expressions", you blur the frontier between code and data.}
  @para{<insert server+html+js code here>})

(slide #:title "Come make your language today!"
  @para{})
