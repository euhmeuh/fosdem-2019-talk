#lang brag
ranch: [/NEWLINE] /RANCH-BEGIN /NEWLINE ponies /RANCH-END [/NEWLINE]
ponies: pony*
pony: /SPACE* /PONY-BEGIN /SPACE+ name /SPACE+ cry /NEWLINE
@name: ID
@cry: STRING
