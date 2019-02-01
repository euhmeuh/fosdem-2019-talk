#lang at-exp slideshow

(require
  pict/color
  slideshow/text
  slideshow/code
  rsvg)

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

(define (cpara . args)
  (apply para #:align 'center args))

(define (separator) (hline 200 50))

(define (title txt [subtitle #f])
  (vc-append 20
    (titlet txt)
    (if subtitle
        (small (para #:align 'center subtitle))
        (blank))
    (separator)))

(define (code-example . txt)
  (small (codeblock-pict (apply string-append txt))))

(define (text-block . txt)
  (blue (small
    (apply vl-append
      (for/list ([line (in-lines
                         (open-input-string
                           (apply string-append txt)))])
        (tt line))))))

(define (racket-title)
  (vc-append
    (hc-append 10
      (svg-file->pict "racket-logo.svg" 0.25)
      (vl-append 5
        (big (t "Racket"))
        (small (t "The language-oriented programming language"))
        (small (blue (t "racket-lang.org")))))
    (separator)))

(define (github-project name)
  (small (para (hb-append (t "Full project: ")
                          (blue (tt (string-append "github.com/" name)))))))

(define (id-card)
  (hc-append 10
    (scale (bitmap "jerome.jpg") 0.5)
    (vl-append 5
      (t "Jérôme Martin")
      (t "Web developer at OVH")
      (t "Racket contributor")
      (blue (tt "github.com/euhmeuh"))
      (tt "\"jerome\" on racket.slack.com"))))

(define code-examples (list
@code-example|{
{
  "name": "My pony ranch",
  "owner": {
    "name": "Jerome",
    "surname": "Martin",
  },
  "ponies": [
    { "name": "Rarity", "level": 3 },
    { "name": "Applejack", "level": 2 },
    { "name": "Twilight", "level": 2 },
  ]
}
}|

@code-example|{
{
  "name": "My pony ranch",
  "owner": {
    "name": "Jerome",
    "surname": "Martin",
  },
  "ponies": [...]
}
}|

@code-example|{
{
  "name": "My pony ranch",
  "owner": "1234", ← here
  "owners": {
    "1234": { ← here
      "name": "Jerome",
      "surname": "Martin"
    }
  },
  "ponies": [...]
}
}|

@code-example|{
{
  "name": "My pony ranch",
  "maxLevel": 3,
  "ponies": [
    { "name": "Rarity", "level": 3 },
    { "name": "Applejack", "level": 2 },
    { "name": "Twilight", "level": 2 },
  ],
  "numberOfMaxLvlPonies": ???, ← cannot be computed with JSON only
}
}|

@text-block|{
// sample Grunt file from any JS project
module.exports = function (grunt) {
  "use strict";
  require("matchdep").filterAll("grunt-*").forEach(grunt.loadNpmTasks);

  grunt.initConfig({
    pkg: grunt.file.readJSON("package.json"),
    distdir: "dist",
    srcdir: "src",
    transdir: ".work/.trans",
    testdir: ".test/",
    builddir: ".work/.tmp",
    name: grunt.file.readJSON("package.json").name,

  // thousands of lines ...
}|

@code-example|{
#lang racket/base
(provide (except-out (all-from-out racket/base)
                     #%module-begin)
         (rename-out [module-begin #%module-begin])

(define-syntax-rule (module-begin stx)
  (#%module-begin 'stx)) ; ← insert your logic here
}|

@code-example|{
#lang web-galaxy
(response (pony id)
  (define the-pony (get-pony-by-id id)) ; ← database fetching
  (html
    (head
      (style
        (.pony (border 1 'solid 'pink) ; ← CSS
               (background-color 'dark-pink))))
    (body
      (div ([class "pony"]) (pony-name the-pony)) ; ← HTML
      (javascript
        (function (feed-pony elt) ; ← Javascript
          (add-class elt "fed"))))))
}|

@code-example|{
#lang virtual-mpu/mpu
(mpu "6802"
  (registers (a b sr [ix 16] [sp 16] [pc 16]))
  (status sr (carry overflow zero negative interrupt half))
  (interrupts interrupt [irq #xFFF8]
                        [soft #xFFFA]
                        [nmi #xFFFC]
                        [restart #xFFFE])
  (operations
    ;; branches
    (bcc "Branch if carry clear" (rel) (branch (carry?) rel))
    (bcs "Branch if carry set" (rel) (branch (not (carry?)) rel))

    ;; other operations...
    ))
}|

@code-example|{
#lang scribble/base

@title{On the Cookie-Eating Habits of Mice}

If you give a mouse a cookie, he's going to ask for a
glass of milk.

@section{The Consequences of Milk}

That ``squeak'' was the mouse asking for milk. Let's
suppose that you give him some in a big glass.
}|

))

(define (example n) (list-ref code-examples n))

(define (side-by-side a b)
  (hc-append 20 a (arrow 30 0) b))

;; --- Begin slides ---

(slide
  (title "FOSDEM 2019" "Minimalistic Languages Devroom")
  (title "Why JSON when you can DSL?"
         "Creating file formats & languages that fit your needs")
  @cpara{@bt{Jérôme Martin}}
  @item{Developer at OVH (hosting/datacenters)}
  @item{Former developer at Ubisoft (video games)})

(slide
  (title "What we expect from JSON")
  @para{JSON is used to represent structured data:}
  'next
  (example 0)
  'next
  @para{It allows some types only: strings, numbers, arrays, dicts... that's it.})

(slide
  (title "What if we want self-references?")
  'next
  @para{We need to add IDs:}
  (side-by-side (example 1) (example 2))
  @para{Good luck to keep them in sync...})

(slide
  (title "What if we need to count some elements?")
  (example 3)
  @para{We need a program to process our data.})

(slide
  (title "JSON is not enough" "In fact, data structures are not enough!")
  'next
  @item{They fail at self-referencing.}
  'next
  @item{They fail at representing processes and computations.})

(slide
  (title "What does everyone do?")
  'next
  @para{Let's add javascript to it!}
  'next
  (example 4)
  'next
  @para{What were simple data files have become monsters. WHY? :(})

(slide
  (title "How data structures betrays us")
  @para{"A data structure is just a stupid programming language" -- Bill Gosper}
  'next
  @para{JSON, XML, HTML, CSS... are all stupid programming languages.}
  'next
  @para{When we try representing concepts with them, abstraction inherently leaks.}
  @para{We always end up writing the missing abstraction layer by hand.})

(slide
  (title "In need for meta")
  @para{We try to make them less stupid:}
  @item{CSS → Less}
  @item{Javascript → Babel}
  @item{HTML → Mustache templates / JSX syntax}
  @item{C → Preprocessor}
  @item{C++ → Templates}
  @item{Python → meta-classes}
  @item{C# → Reflection})

(slide
  (title "What if I told you")
  @cpara{There's a simpler way.}
  @cpara{There's a more personal way.})

(slide
  (title "The DSL way")
  @cpara{Domain Specific Languages}
  'next
  @para{→ The abstraction becomes a language.}
  'next
  @para{Examples: Makefile, Regexps, SQL, Qt, GameMaker Language...}
  @para{All those languages are used to represent complex data structures.})

(slide
  (title "The DSL way")
  @para{Structured data will always be better expressed with a specific language for that domain than a generic data structure.}
  'next
  @item{Banking data → banking language}
  @item{Game data → game language}
  @item{Medical data → medical language})

(slide
  @cpara{But isn't writing a full language excessive?})

(slide
  (title "Rule #1: Abstraction leaks")
  @para{Any data eventually becomes a DSL *naturally* by leaking through abstractions.}
  @cpara{→ Natural growth}
  'next
  @para{Most programs are tools made to prevent this leakage (a.k.a. "middleware").}
  'next
  @para{Only most of the time they become a badly written half implementation of lisp.}
  'next
  @para{So why not using lisp in the first place?})

(slide
  (racket-title)
  'next
  @para{As a lisp language, it allows writing itself *by design*.}
  'next
  @para{Racket is specialized in writing Domain Specific Languages (DSL):}
  @item{#lang slideshow}
  @item{#lang racket/gui}
  @item{#lang scribble}
  @item{#lang video}
  @item{#lang web-server})

(slide
  (title "How to make a DSL")
  @para{It takes 5 lines of Racket to implement a generic parser for any language:}
  (example 5)
  (separator)
  @para{Save those lines in a file @it{"my-lang.rkt"} and you got yourself a full @bt{reader}, @bt{parser} & @bt{expander} with all the standard functions from Racket.})

(slide
  (title "Code == Data")
  @para{Using the lisp syntax called "s-expressions", you blur the frontier between code and data.}
  (example 6)
  (separator)
  (github-project "euhmeuh/web-galaxy"))

(slide
  (title "Code == Data")
  (example 7)
  (separator)
  (github-project "euhmeuh/virtual-mpu"))

(slide
  (title "Scribble" "The Racket documentation language")
  (example 8))

(slide
  (title "Take away")
  'next
  (small @item{When data structures are not enough, we seek more abstraction.})
  'next
  (small @item{DSLs are the best way to express those abstractions as languages.})
  'next
  (small @item{Racket is a language specialized in writing languages.})
  'next
  (small @item{By putting your "middleware" logic into a language, you make sure it can evolve and always fit your domain.})
  'next
  (small @item{You don't have to write ugly data transformation scripts ever again!})
  'next
  (small @item{You provide your team with a single piece of documentation concerning your domain: the language spec.}))

(slide
  (title "Come make your language today!")
  @para{Join me at the booth @bt{K.4.201} at @bt{17:20} to make your own language!}
  @para{Yes, you can actually make your own language in an hour with Racket!}
  @para{See you there!}
  (id-card))
