#lang brag

ranch: RANCH-BEGIN /SPACE+ /NEWLINE ponies RANCH-END
ponies: (/SPACE* PONY-BEGIN /SPACE+ name /SPACE+ cry /NEWLINE)*
@name: ID
@cry: STRING
