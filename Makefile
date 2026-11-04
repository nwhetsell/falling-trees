all: falling-trees.pdf

dates.ily : info.json
	echo "#(define copyright-date (car (strptime \"%F\" $$(jq '."copyright date"' info.json))))" > dates.ily
	echo "#(define revision-date (car (strptime \"%F\" $$(jq '."revision date"' info.json))))" >> dates.ily

falling-trees.pdf : falling-trees.ly dates.ily
	lilypond falling-trees.ly

single-system.png : falling-trees.ly
	lilypond --loglevel=ERROR --output=single-system --png --define-default=resolution=300 - <<< '\
	  \layout { \
	    \context { \
	      \Score \
	      centerBarNumbers = ##t \
	      barNumberVisibility = #all-bar-numbers-visible \
	      \override CenteredBarNumberLineSpanner.direction = #DOWN \
	    } \
	  } \
	  \include "falling-trees.ly" \
	  \paper { \
	    page-breaking = #ly:one-line-auto-height-breaking \
	    top-system-spacing = #f \
	    indent = 0 \
	    left-margin = 0.25\in \
	  } \
	'
	rm -f single-system.midi

clean :
	rm -f \
	dates.ily \
	falling-trees.pdf \
	single-system*.png
