#!/bin/sed -f

1,2 s/$/./
3,$ s/ /,/g
3,$ s/^/[/
3,$ s/$/]./
