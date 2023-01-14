\version "2.24.0"

\header {
  title = "Falling Trees"
  composer = "Nathan Whetsell"
}

\language "english"

#(define subdivide-beams (define-music-function (baseMoment beatStructure music) (rational? list? ly:music?)
#{
  \set subdivideBeams = ##t
  \set baseMoment = #(ly:make-moment baseMoment)
  \set beatStructure = #beatStructure
  #music
  \unset beatStructure
  \unset baseMoment
  \unset subdivideBeams
#}))

\score {
  \new PianoStaff <<
    \new Staff="up" \relative {
      \accidentalStyle piano
      \key g \minor

      \tempo 4=112
      \time 4/4
      \ottava #1 c''''8( \tuplet 3/2 { df16 bf df } ef8.) \subdivide-beams #1/8 2 { bf16 16( g \tuplet 3/2 { e' a, g } } e'8.) df,16 |
      \tuplet 6/4 4 { df'16 bf fs c ef a, df fs g ef g a bf, df g bf, df fs bf, e bf g a ef' } |
      \time 3/4 \tuplet 6/4 { ef,16 fs' bf, ef, df' ef~ } \subdivide-beams #1/8 2 { \tuplet 3/2 8 { 16 g, df'~16 e8 } } <e e,>~ \tuplet 3/2 { 16 <ef gf,>8 } |
      \time 2/4 <df bf>4-> \ottava #0 ef,16 bf' ef,32( e g a) |
      \time 3/4 <bf bf,>4-^ ef, \acciaccatura g16 ef8 a |
      \time 4/4 g16 ef16 \change Staff="down" g,,8 \change Staff="up" c'16 bf df \change Staff="down" e, \change Staff="up" r8 \stemUp \tuplet 3/2 { df'8 g,16 } \stemNeutral \change Staff="down" e,8 \change Staff="up" c'' |
      bf8. g16 g4 a2 |
      R1
      r2 r4 \ottava #1 g''4 |
      fs16( g df') \change Staff="down" df, \change Staff="up" \tuplet 6/4 { c' \change Staff="down" df, e df \change Staff="up" bf e, } \tuplet 3/2 { \change Staff="down" c bf' \change Staff="up" c' } c8 \ottava #0 r4 |
      \time 3/4 \ottava #2 \tuplet 3/2 8 { g'8[ bf,16] \change Staff="down" c,8[ \change Staff="up" fs'16] df8 \once \override Stem.beaming = #(cons (list 0 1) (list 0)) bf16 } c8 r4 |
      \time 2/4 \tuplet 6/4 { bf'16 df, bf bf' bf, \change Staff="down" e,16 \change Staff="up" } r4 |
      \time 4/4 c''16 e, e bf' c2. |

      R1\fermata \bar "||"

      % A
      \mark \default
      \tempo 4=78
      \time 3/4
      <c bf e, df>4-> e,8 a ef-> <df a> |
      <g g,>8.-> <e fs,>16~8 a,-> bf4 |
      \time 4/4
      \ottava #1 <a bf,>4.-> df,8 <g c,>4 <c, g fs> |
      <fs df c a>4 <e df bf>8. <ef df>16~8 <a, g ff ef>8~16 <fs bf,>8. |
      <e' e,>4 <ef ef,>8. \ottava #0 <a, g c, bf>16~8 <bf ef, df> <fs bf,> <e c> |
      <ef df>8. <fs c>16~8 <g e df bf>8~4 \clef "bass" <e, df bf g e> \clef "treble" |
      \set PianoStaff.connectArpeggios = ##t
      \override PianoStaff.Arpeggio.arpeggio-direction = #UP
      <g' fs e bf>8.\arpeggio ef16~8 <ef g,>~ \tuplet 3/2 4 { 8 df-> <bf ef,>-> <a g ef>-> c-> df-> } |
      \time 3/4 <df bf a>8-> df c \grace df16 e8 g <gf df c> |
      \time 4/4
      <bf a fs>8. bf16~8 <c bf g>8 r2 |
      \ottava #1 bf4 <ef df bf>2. |
      <df c>1 |
      r2 r4 df'~ |
      \time 3/4 4 bf2 | \bar "||"

      % B
      \mark \default
      \tempo 4=104
      \time 4/4
      \ottava #2 c'8 bf \change Staff="down" g,16 \change Staff="up" e'32 \change Staff="down" bf32 g8~8 \change Staff="up" \tuplet 3/2 { a'16 \change Staff="down" bf, g \change Staff="up" } e' ef8. |
      g16 ef df8~8 e16 \change Staff="down" a, \change Staff="up" \ottava #0 s2 |
      \time 5/4
      \ottava #1 \tuplet 3/2 { bf8 df, bf ~ } bf4 df16 c bf a c a fs \change Staff="down" bf, \change Staff="up" e' df a8 |
      \tuplet 6/4 4 {
        df16 bf e, df \change Staff="down" g, df \change Staff="up"
        e''16 g, ef df \change Staff="down" bf e, \change Staff="up"
        \stemUp ef''16 \tweak Y-offset #6 ^\markup { \italic "molto rit." } df a g \change Staff="down" df \change Staff="up" a' \stemNeutral
        g16 \change Staff="down" e ef df a \change Staff="up" e'
      } \tuplet 3/2 { bf8 \change Staff="down" e, df \change Staff="up" } |
      \tempo "a tempo"
      <e'' e,>4 e \tuplet 3/2 { e8 df ef, } <a df,>16 g c, \change Staff="down" g, \change Staff="up" \ottava #0 ef'' df gf, \change Staff="down" df \change Staff="up" |
      \time 7/8 bf'16 gf' \tuplet 3/2 { ef16 \change Staff="down" e, a, \change Staff="up" } \tuplet 3/2 { \change Staff="down" fs \change Staff="up" gf'' df~ } 8 \tuplet 3/2 { g16 e df } \tuplet 3/2 { bf16 a' e~ } 8~ |
      \time 3/4 8 g16 c, \change Staff="down" fs, e df \change Staff="up" \clef "bass" g, e \change Staff="down" a, \change Staff="up" \clef "treble" bf'' \change Staff="down" a,,, \change Staff="up" |
      \time 3/8 \tuplet 3/2 { g''''8 e ef~ } 8\noBeam |
      \time 3/4
      ef8 g,16 \change Staff="down" g,16 \change Staff="up" \tuplet 3/2 { a'8 g \change Staff="down" ef \change Staff="up" } df'16 c df \change Staff="down" df, \change Staff="up" |
      \change Staff="down" c,16 \change Staff="up" e'' bf df c8~ \tuplet 3/2 { 16 df e } \tuplet 3/2 { \change Staff="down" fs, \change Staff="up" bf g' } \change Staff="down" g,,8 \change Staff="up" |
      \time 4/4
      \change Staff="down" \tuplet 3/2 8 { bf16 a8 \change Staff="up" gf''16 df \change Staff="down" bf, \change Staff="up" } bf'' a gf \change Staff="down" g, df \change Staff="up" bf'' e, ef~4 |
      \ottava #1 c'16 bf8.~4 \tuplet 3/2 { \change Staff="down" e,,8 \change Staff="up" df'' bf } e, \change Staff="down" g,, \change Staff="up" |
      s2. c''16 ef,8. |
      \change Staff="down" ef16 bf \change Staff="up" \tuplet 3/2 { df'16 c8~ } 4~2~ |
      \time 6/4 4 \tuplet 3/2 { \change Staff="down" c,8 \change Staff="up" df' \change Staff="down" fs, \change Staff="up" g' \change Staff="down" g,4~ } 2. \change Staff="up" | \bar "||"

      % C
      \mark \default
      \time 3/4 a'16 c, \change Staff="down" g8 \change Staff="up" df'16 bf a g <df' fs,> bf ef, \change Staff="down" ef, \change Staff="up" |
      \time 4/4 ef''16 c bf g e \change Staff="down" c a g \change Staff="up" \ottava #0 \tuplet 6/4 { bf'16 g fs e \change Staff="down" ef c \change Staff="up" } \tuplet 6/4 { bf'16 g fs e \change Staff="down" df bf \change Staff="up" } |
      \time 3/4
      g'16 gf ef \change Staff="down" fs, \change Staff="up" bf' e, bf \change Staff="down" g \change Staff="up" df' \change Staff="down" bf, \change Staff="up" df'' bf |
      \tempo \markup { \normal-text { \italic "poco rit." } }
      \tuplet 3/2 4 { g8 \change Staff="down" fs, df \change Staff="up" df' \change Staff="down" g, e \change Staff="up" df' bf \change Staff="down" a \change Staff="up" } |
      g'16 df c \change Staff="down" ef, s4 \tuplet 6/4 { bf16 g' \change Staff="up" \crossStaff { ef' } \change Staff="down" bf, \change Staff="up" g'' \change Staff="down" e, \change Staff="up" } |
      \time 4/4
      \tuplet 6/4 4 { \change Staff="down" g,16 e ef \change Staff="up" e'' ef df \change Staff="down" a,, \change Staff="up" g'' \change Staff="down" bf, df, \change Staff="up" a'' \crossStaff { e } } \tempo 4=96 c4 bf'16 e, df \change Staff="down" a, \change Staff="up" | \bar "||"
      \tempo \markup { \normal-text { \italic "molto rit." } }
      \key e \major
      cs''!16 a fs \change Staff="down" cs \change Staff="up" e' \change Staff="down" cs,! c gs c, \change Staff="up" e' fs c' \tuplet 6/4 { e <ds! a> ds a \change Staff="down" c, a \change Staff="up" } |
      \tempo "a tempo" 4=104
      a''16 \dim gs \! e \change Staff="down" a, \change Staff="up" fs' e gs, \change Staff="down" c, \change Staff="up" a''32 cs a8. <a c,>4 |
      a4 \ottava #1 s s \tuplet 3/2 { gs'8 fs ds } |
      \time 5/4 gs8 cs, a' e \tuplet 3/2 { gs8 a, gs } \tempo \markup { \normal-text { \italic "rit." } } gs'16 c, a gs' ds a8. |
      \time 4/4
      \tempo 4=72
      \tuplet 3/2 { c'16 a gs16~ } 8~4~8~ \tuplet 3/2 { 16 a gs~ } 4 | \bar "||"

      % D
      \mark \default
      a8 4 8~2 |
      r2 r4 cs16 e,8.~ |
      \time 6/4 16^\markup { \italic "accel." } cs8.~4 \ottava #0 \tuplet 3/2 { gs,8 c gs'~ } 4 \tuplet 3/2 4 { gs,8[ a cs~] 8 fs, \change Staff="down" gs, \change Staff="up" } |
      \time 4/4
      \tempo "a tempo" 4=104
      cs'16 fs, \change Staff="down" gs, \change Staff="up" cs' c gs e a e'8 cs16 \change Staff="down" a, \change Staff="up" a' fs ds \change Staff="down" gs, |
      \tuplet 6/4 4 { e16 \change Staff="up" a' gs \change Staff="down" ds, cs' c \change Staff="up" cs' gs e e'16 ds \change Staff="down" fs, } fs, \change Staff="up" c'' cs, \change Staff="down" cs, ds \change Staff="up" c' \change Staff="down" a,8 \change Staff="up" |
      \time 3/4
      ds''16 cs gs \change Staff="down" e, \change Staff="up" \tuplet 3/2 { a'8 e \change Staff="down" ds, \change Staff="up" } s4 |
      <ds''! c cs,!>4->^\markup { \italic "accel." } \tuplet 3/2 { \tweak Y-offset #6 \ottava #2 a'''8-^ \ottava #0 \change Staff="down" a,, gs \change Staff="up" } s4 |
      r4 \tuplet 3/2 { a,8 e \change Staff="down" cs \change Staff="up" } cs'16 \change Staff="down" a \change Staff="up" e' \change Staff="down" ds, \change Staff="up" |
      \tempo 4=144
      \tuplet 3/2 4 { ds'8 \change Staff="down" a fs \change Staff="up" cs' \change Staff="down" a c, \change Staff="up" } gs''16 cs, a' e |
      \tempo \markup { \normal-text { \italic "rit." } }
      gs16 e ds \change Staff="down" c \change Staff="up" \tuplet 3/2 { a'8 gs gs } a \change Staff="down" gs, \change Staff="up" |
      \time 4/4
      \ottava #1 cs'8 a ds4 \tuplet 3/2 { e4 ds8 } \tuplet 3/2 { fs4 e8 } |
      \tempo 4=129
      gs16 cs, \change Staff="down" e,16 a, gs e' \change Staff="up" gs \change Staff="down" c,~8 \change Staff="up" a'16 gs \change Staff="down" a,8 \change Staff="up" gs''16 gs, |
      \time 3/4
      \tempo \markup { \normal-text { \italic "molto rit." } }
      r4 \tuplet 3/2 4 { gs'8 c a } c,4 |
      a'16 \change Staff="down" gs, gs,8~2 \change Staff="up" |
      \tempo 4=72
      a''16 a,8. a'16 b,8.~4 |
      gs'16 a,8. c'16 c,8.~4 |
      \time 4/4
      a'16 b,8. cs16 gs8.~8 gs16 a~8. \change Staff="down" gs,16 \change Staff="up" |
      fs'8. \change Staff="down" a,16 \change Staff="up" a'8 gs'~8 c, s8. \ottava #0 s16 |
      \tempo \markup { \normal-text { \italic "poco accel." } }
      \time 5/4 a4 \change Staff="down" gs,16 cs \change Staff="up" a'8~4~2 | \bar "||"

      % E
      \mark \default
      \tempo 4=156
      \time 3/4 r4 e'8 a, cs4 |
      \time 4/4
      \tuplet 3/2 { \change Staff="down" e,,8 b' \change Staff="up" cs' } cs4 \grace { cs,16 } a'4 \tempo \markup { \normal-text { \italic "rit." } } \tuplet 3/2 { b,8 fs cs } |
      \tempo "a tempo" 4=104
      \change Staff="down" a,16 ds \change Staff="up" e' \change Staff="down" a,,, \change Staff="up" \tuplet 3/2 { ds'''8 e, \change Staff="down" a,, \change Staff="up" } a''16 cs, \set tieWaitForNote = ##t cs'~ \change Staff="down" gs,, \change Staff="up" cs''4 \unset tieWaitForNote |
      \tempo \markup { \normal-text { \italic "poco più mosso" } } 4=120
      a16 \change Staff="down" gs, cs a \change Staff="up" e'' ds a \change Staff="down" gs, \change Staff="up" s4 a'16 \change Staff="down" cs, \change Staff="up" gs'' ds |
      \time 3/4 \change Staff="down" a,16 gs \change Staff="up" gs'' a \tuplet 3/2 { a,8 fs' e } s4 |
      \tempo \markup { \normal-text { \italic "rit." } }
      \time 3/2 \ottava #1 cs'8 \change Staff="down" a, \change Staff="up" a' \change Staff="down" cs, \tuplet 3/2 { e, gs \change Staff="up" b' } e16 a, \crossStaff { cs } ds, \crossStaff { b'16 } \change Staff="down" <e, fs,> \change Staff="up" fs' \change Staff="down" ds, \change Staff="up" gs' e ds \change Staff="down" cs, \change Staff="up" |
      \tempo "a tempo" 4=104
      \time 4/4
      \tuplet 7/4 { \change Staff="down" a,16 \change Staff="up" gs''' \change Staff="down" a,, gs \change Staff="up" ds'' a' \change Staff="down" cs,, } %{\once \override TupletBracket.positions = #'(-4 . -4)%} \tuplet 3/2 4 { fs,8 \change Staff="up" a' \change Staff="down" \stemUp ds,~8 \stemNeutral \change Staff="up" a'4 \tupletNeutral gs'8 gs, \change Staff="down" e \change Staff="up" } |
      r4 s2 \tuplet 3/2 { a'4 fs8~ } |
      2 gs16 \change Staff="down" a,8 \change Staff="up" b'16 a8 b16 fs |
      r2 r4 r8. ds16 |
      cs2~8. e16 r4 |
      r4 fs4 ds2~ |
      8. \change Staff="down" cs,16 \change Staff="up" s4 s s8 \tuplet 3/2 { \change Staff="down" cs16 ds \change Staff="up" cs'' } | \bar "||"

      % F
      % \once \override Score.RehearsalMark.outside-staff-priority = #390
      \mark \default
      \time 5/4 r2. \change Staff="down" a,,16 ds \change Staff="up" ds'8~4 |
      \tempo \markup { \normal-text { \italic "accel." } }
      \time 6/4 \change Staff="down" gs,,,16 \change Staff="up" e'' gs8~4 \change Staff="down" b,,16 \change Staff="up" ds'16 ds'8~4 \tuplet 3/2 { \change Staff="down" gs,,,16 gs' \change Staff="up" cs' } b8~16 gs8. |
      \tempo 4=155
      \time 4/4 cs4\arpeggio gs\arpeggio \tuplet 3/2 4 { \change Staff="down" cs,,8 \change Staff="up" b'' gs a \ottava #0 \change Staff="down" a,, b \change Staff="up" } |
      \tempo \markup { \normal-text { \italic "rit." } }
      \time 6/4 \tuplet 3/2 4 { cs'8[ gs' \change Staff="down" b,,8] a[ \change Staff="up" ds' cs] } \change Staff="down" gs,[ ds' e \change Staff="up" a] \tuplet 3/2 4 { ds[ fs, b] e[ gs, \change Staff="down" a,] \change Staff="up" } |
      \tempo "a tempo" 4=104
      \time 3/4
      \change Staff="down" ds16 \change Staff="up" <fs' ds> e \change Staff="down" gs,, ds' \change Staff="up" cs' e \change Staff="down" a, \change Staff="up" \grace ds16 ds'8 \change Staff="down" cs, \change Staff="up" |
      \tempo \markup { \normal-text { \italic "accel." } }
      \change Staff="down" cs,16 fs, \change Staff="up" e'' ds \tuplet 3/2 { \change Staff="down" gs,8 \change Staff="up" a' gs } a,16 \change Staff="down" c, e, \change Staff="up" ds'' |
      \time 5/4
      \tempo \markup { \normal-text { \italic "rit." } }
      \ottava #1 gs16 \change Staff="down" e, \change Staff="up" a' \change Staff="down" e, \change Staff="up" s4 \tuplet 3/2 { e''16 gs, cs, } ds'8 e16 cs b gs~4 |
      gs'16 cs, \change Staff="down" e, a, fs \change Staff="up" a' fs \change Staff="down" a, \change Staff="up" gs' e \change Staff="down" c a \change Staff="up" e''4 e, |
      \tempo "a tempo"
      \time 4/4
      r8 gs'16 e a fs~8~2~ |
      8. gs16 fs4~8. cs16~8. a'16~ |
      1 |
      b1~ |
      2 cs,2 |
      e2. cs4 |
      \tempo \markup { \normal-text { \italic "rit." } }
      r8 fs,4.~8 cs' gs'4 | \bar "||"

      % G
      \mark \default
      \tempo 4=78
      s4 \tuplet 3/2 { ds8 a4~ } 4 c16 e c, \ottava #0 \change Staff="down" e,, \change Staff="up" |
      \tempo "a tempo" 4=104
      r16 e'16( gs fs ds' fs8) a,16( cs e a gs ds8) cs16 e |
      \time 3/4 fs4 \subdivide-beams #1/8 2 { \tuplet 3/2 { \change Staff="down" ds,16 \change Staff="up" ds' gs } a \change Staff="down" e, } fs \change Staff="up" fs' a, gs' |
      \time 4/4
      \tempo \markup { \normal-text { \italic "rit." } }
      r16 e,( gs fs cs' e8) gs,16 \change Staff="down" fs, cs' \change Staff="up" a' fs'-> \change Staff="down" cs,, a' \change Staff="up" e'16 d'->~ |
      \tempo 4=78
      16 cs, e c'->~16 gs, c a' \tempo \markup { \normal-text { \italic "accel." } } \tuplet 3/2 4 { <cs fs,>8-> fs,, gs cs' a <gs e>~ } |
      \tempo "a tempo" 4=104
      4 \change Staff="down" e'8 \change Staff="up" \ottava #1 \tweak TupletNumber.X-offset #1 \tweak TupletNumber.Y-offset #-4.5 \tuplet 3/2 { c''16 gs \change Staff="down" a, \change Staff="up" } \change Staff="down" a,8 \change Staff="up" \tuplet 3/2 { ds''16 c e,~ } 4~ |
      8. e16~4~8 a~ \tuplet 3/2 { 8 ds, a~ } |
      \tuplet 3/2 { 8 cs c~ } 4 cs2 |
      fs16 c8.~8 gs'16 gs,~2 \ottava #0 |
      \tempo \markup { \normal-text { \italic "molto rit." } }
      e'2 a,~ | \bar "||"

      % H
      \mark \default
      \time 5/4 2 fs8. gs,16 \tempo 4=46 a'8 a16 a, <a' fs>4 |
      \tempo \markup { \normal-text { \italic "accel." } }
      \time 6/4 b,16 e'8 \change Staff="down" b,,16 \change Staff="up" cs'' gs a cs, \change Staff="down" gs,16 ds' \change Staff="up" fs' gs \change Staff="down" cs,, \change Staff="up" e' a cs \change Staff="down" a,, gs' a \change Staff="up" b' ds, \change Staff="down" cs, ds e \change Staff="up" |
      \tempo "a tempo" 4=104
      \time 4/4 cs'16 e a, gs \subdivide-beams #1/8 2 { \tuplet 3/2 { cs16 a e' } } fs,32( gs a ds32) 4 e |
      \time 3/4 ds16 cs e fs ds \change Staff="down" ds, e a, cs, \change Staff="up" gs'' e' e, |
      \time 4/4
      \tuplet 3/2 4 { ds8 e cs a' c gs } \change Staff="down" f,,16 c' \change Staff="up" c' e cs' \change Staff="down" e,, a, \change Staff="up" cs' |
      f'16-> e, \change Staff="down" c \change Staff="up" a' \tuplet 3/2 { e'8-> f-> gs,-> } a'4 \tuplet 3/2 { a,8 c e, } |
      \time 3/4 gs4 \tuplet 3/2 4 { fs'8 gs <e c> <fs ds>-> <gs c,>-> <a ds,>-> } |
      \time 5/4 ds,16 \change Staff="down" a,8 \change Staff="up" a'16 \change Staff="down" c, cs \change Staff="up" cs' gs' \change Staff="down" ds, e gs \change Staff="up" a' r4 \tuplet 6/4 { \change Staff="down" cs,16 \change Staff="up" ds a' c \change Staff="down" a, \change Staff="up" ds } |
      \time 4/4
      \tuplet 3/2 { cs'8 \once \override Stem.beaming = #(cons (list 0 1) (list 0)) e,16 } c'8~8. e16~4~16 ds,8 ds16~ |
      8. ds'16~8. cs16 a4.. \ottava #1 cs16 |
      \tempo \markup { \normal-text { \italic "rit." } }
      c2~8. a16 fs'8. gs,16 |
      \tempo 4=84
      gs'4.. gs,16 e'8 \change Staff="down" cs, \tuplet 3/2 { gs16 c ds } \change Staff="up" a'8~ | \bar "||"

      % I
      \mark \default
      \time 5/4
      4 r4 \tuplet 3/2 { \change Staff="down" gs,16 ds' \change Staff="up" ds' } ds8 ds4 fs, |
      \tempo \markup { \normal-text { \italic "accel." } }
      \tuplet 3/2 { e'8 \change Staff="down" a,, \change Staff="up" a' } fs'16 fs cs8 \change Staff="down" gs,16 \change Staff="up" fs' ds' e, \tuplet 3/2 { ds'8 \change Staff="down" ds, e, \change Staff="up" } r gs' |
      \tempo \markup { \normal-text { \italic "rit." } }
      <<
        { ds'8 cs ds e cs e b gs fs4 }
        \\
        { s1 \once \override Beam.positions = #'(-6 . -5) \tuplet 6/4 { fs16 \change Staff="down" \stemUp cs b e, \stemNeutral \change Staff="up" cs' cs' } }
      >> |
      \tempo 4=75
      \ottava #0 \stemUp gs4 \stemNeutral \tuplet 3/2 { b8 e, \change Staff="down" gs,, } e16 ds' \change Staff="up" e' cs \tuplet 6/4 { a' b, gs' \change Staff="down" fs, \change Staff="up" fs' ds } \tuplet 3/2 { gs8 ds b } |
      \tempo \markup { \normal-text { \italic "accel. poco a poco" } }
      cs4 r2 r |
      r4 a8 \change Staff="down" fs, \change Staff="up" b'16 \change Staff="down" gs, e \change Staff="up" ds''~4 e16 \change Staff="down" a, \change Staff="up" gs' \change Staff="down" gs, \change Staff="up" |
      \time 4/4
      \change Staff="down" b16 \change Staff="up" b' e, e, \tuplet 6/4 4 { \change Staff="down" e,16 a ds gs, \change Staff="up" fs' gs gs' e ds \change Staff="down" a, b ds \change Staff="up" } \tuplet 3/2 { \change Staff="down" fs \change Staff="up" cs' gs } gs'8 |
      \tempo "a tempo" 4=104
      \ottava #1 ds'8 e ds a cs e b8. gs16 |
      fs4 r \tuplet 6/4 { a8 fs16 gs' \change Staff="down" cs,,,16 cs' \change Staff="up" } r16 cs'16 ds32 e ds gs |
      a1 |

      R1\fermata \bar "||"

      % J
      \mark \default
      \time 3/4
      r16 gs8.~4~16 gs8.~ |
      16 ds8.~2~ |
      16 b8.~2~ |
      16 e8.~4~8 gs~ |
      4~16 gs8.~4~ |
      16 ds8.~2~ |
      16 b8.~16 a8.~4~ |
      16 e'8.~4~8 gs8~ |
      2~16 gs8.~ |
      \time 5/4 16 ds8.~2~16 b4..~ |
      \time 3/4
      16 a8.~4~16 e'8.~ |
      8 gs~2~ |
      \time 4/4
      16 gs8.~16 ds8.~2~ |
      16 b8.~4~2 \ottava #0 \bar "|."
    }

    \new Dynamics {
      s1 \pp
      \override DynamicTextSpanner.style = #'none
      s1 \cresc
      s2.
      s2 \f \<
      s2. \ff \>
      s4 \mf s2. \tweak Y-offset #-1 \dim
      s1 \p \>
      s1 \pp
      s1*2 \cresc
      s2 \mf s4 \dim
      s2
      s1*2 \pp

      % A
      s2. \pp
      s2. \cresc
      s1*3
      s2. s4 \ff
      s4. \fff \> s8 \f s2
      s2. \dim
      s1
      s1*3 \p
      s2.

      % B
      s1*2 \pp
      s1*5/4*2 \cresc
      s4 \f s1 \cresc
      s1*7/8
      s2. \ff
      s4. \dim
      s2.*2
      s1
      s1*3 \pp
      s1.

      % C
      s2. \pp
      s1
      s2.*3 \tweak Y-offset #-6 \cresc
      s2 s \ff
      s1
      s1*2 % \dim
      s1*5/4
      s1 \pp

      % D
      s1 \pp
      s1 \cresc
      s1.
      s1*2
      s2.
      s2. \ff
      s2. \dim
      s2.*2
      s1*2
      s2.*4 \pp
      s1*2
      s1*5/4

      % E
      s4 \pp s2 \cresc
      s2. s4 \f
      s1
      s2 s \dim
      s2.
      s1.
      s1
      s1*6 \pp

      % F
      s1*5/4 \pp
      s1.
      s1
      s2 s1 \mf
      s2.
      s2. \dim
      s1*5/4*2
      s1*7 \pp

      % G
      s2 \pp s \cresc
      s1 \mf
      s2.
      s1 \cresc
      s2 s4 \ff \> \tuplet 3/2 { s s8 \mp }
      s1 \dim
      s1*4 \tweak X-offset #0.25 \pp

      % H
      s2 \pp s2. \cresc
      s1.
      s1
      s2.
      s4 \f s2. \cresc
      s1 \tweak Y-offset #-4 \ff
      s2. \dim
      s1*5/4
      s1*4 \pp

      % I
      s2. \pp s2 \cresc
      s1*5/4
      s2 \mf \< s4 \> \tuplet 6/4 { s16*5 s16 \! } s4
      s1 s4 \<
      s1*5/4 \mf
      s1*5/4 \cresc
      s1
      s8 \f s \dim s2.
      s1
      s1 \pp
      s1

      % J
      s2.*9 \pp
      s1*5/4
      s2.*2
      s1
      s4 s \> s s16*3 s16 \!
    }

    \new Staff="down" \relative {
      \key g \minor

      r4 ef'2. |
      r8df' c4~ \tuplet 3/2 { 8 e, a } e'4~ |
      4 df8 \tuplet 3/2 8 { <bf gf>16 e, df \subdivide-beams #1/8 2 { e8 <c' e,>16 g bf8 } } |
      a,8.-> a'16 e4 \clef "bass" \acciaccatura { \bar "" e,,16  } | \bar "|"
      <ef c>2-^ r8 c'' |
      s2 \stemDown \tuplet 6/4 { fs16 e g fs e \change Staff="up" g \change Staff="down" } \stemNeutral s4 |
      R1
      e,1 |
      \clef "treble" df'2 bf'' |
      s2. \tuplet 3/2 { r8 r e, } |
      s4 r r8 \tuplet 3/2 { fs16 g e } |
      s4 \tuplet 3/2 { df'16 ef g,~ } 8 |
      R1

      R1\fermata

      % A
      \ottava #1 <c' bf g fs>4-> r8 <a e bf> fs4-> |
      df,4-> r8 <e' c>-> <ef df>4 \ottava #0 |
      <<
        {
          <g, fs df bf>2-> <e df>4 bf~ |
          4 <g e>8. df'16~8 <a g fs>8~<a g fs>8. <df, df,>16 |
        }
        \\
        {
          r8 bf4 \mf c df fs,8~
          8 bf~8 df\laissezVibrer
        }
      >>
      <bf' g e>4 <a ef bf>8. <df, g,>16~8 \clef "bass" <ef df,>8~4 |
      fs,2. \ottava #-1 <df df,>4 |
      <c a,>4.\arpeggio \ottava #0 g'8~g2 |
      r8 e8 bf' fs'8 g,8 bf |
      \clef "treble" <g' df>8. <e' df>16~8 a,8~16 c8. ef4 |
      R1*2
      c1 |
      r4 r8 g'4. |

      % B
      s4 \ottava #1 s2. |
      s2 g'16 bf e, a ef4 \ottava #0 |
      r2. s4 g,, |
      s4 \stemDown gf, bf \stemNeutral s2 |
      df,8 c''~16 bf8.~4 s8. \clef "bass" s16 s4 |
      df,,4 s8 s r r bf'16 e, |
      df,4 \clef "treble" s \clef "bass" s |
      r8 r c'' |
      \stemDown c,4 \stemNeutral s2 |
      s2 \clef "treble" s4 |
      s2. df'16 c a bf |
      df8. c'16 \tuplet 3/2 { a8 ef'8 bf } s2 |
      \change Staff="up" ef'16 e, \change Staff="down" df,8~4~4~16 e8. |
      s2 e'16 df8.~4~ |
      df4 s s1 |

      % C
      s4 r s |
      \stemDown c,4 s df s |
      df4 e fs |
      s2. |
      df4 \stemNeutral <c e,>16 \change Staff="up" bf' a \change Staff="down" df, \once \override TupletNumber.stencil = ##f \tuplet 6/4 { s8 \crossStaff { \stemDown \once \override Stem.length = #0 fs16 \stemNeutral } s8. } |
      \clef "bass" s4 \once \override TupletNumber.stencil = ##f \tuplet 6/4 { s8. s8 \crossStaff { \once \override Stem.length = #0 df,16 } } c4 s |
      \key e \major
      \stemDown fs,4 \stemNeutral s \mergeDifferentlyDottedOn c'8. \mergeDifferentlyDottedOff cs'16 \stemDown c,4 \stemNeutral |
      \clef "treble" s4 s a'2 |
      a'16 gs ds a~8 \change Staff="up" ds''16 cs c a \change Staff="down" e8~4 |
      fs4 c8 ds e,4 gs ds'16 a8. |
      r8 e'4. \acciaccatura cs'16 fs,4 gs4~ |

      % D
      2~8 8 a16 gs cs gs~ |
      8 a16 e~8 ds'16 e,~4 e |
      e1 r4 \tuplet 3/2 { s4 \clef "bass" s8 } |
      s1 |
      s1 |
      s2 \tuplet 3/2 { c,,8 cs c, } |
      fs8-> fs'' \tuplet 3/2 { s \clef "treble" s4 } gs~ |
      8 \clef "bass" a,, \tuplet 3/2 { s \clef "treble" s s } s4 |
      s2 r4 |
      s2. |
      cs''8 a \tuplet 3/2 { fs e cs } e ds e' a, |
      s1 |
      ds2~ \tuplet 3/2 { 8 4 } |
      s2. |
      R2.*2
      r2 r4 s |
      s2 s4 \change Staff="up" b''8. \change Staff="down" gs,,16~ |
      4 s r \tuplet 3/2 { gs8 a4~ } 4 |

      % E
      e8 gs~2 |
      \once \override TupletNumber.stencil = ##f \tuplet 3/2 { s8 \hideNotes b4_~ \unHideNotes } \stemUp 2. \stemNeutral |
      \clef "bass" s2. a,,,4 |
      \clef "treble" s2 ds''8 cs' s4 |
      s4 s \tuplet 3/2 { \change Staff="up" gs'8 \change Staff="down" gs, ds } |
      s2. s8 \crossStaff { \stemDown \once \override Stem.length = #0 cs \stemNeutral } \crossStaff { \stemDown \once \override Stem.length = #0 a'16 \stemNeutral } s8. s4 |
      s1 |
      <<
        { ds2 s2 }
        { s4 \change Staff="up" ds''16 cs gs8~ \tuplet 3/2 4 { 8 \change Staff="down" b, cs,~ } 4 }
      >> |
      fs2 s |
      b1 |
      r2 r4 ds,4~ |
      8. cs16 r4 r2 |
      s4 gs'4~4. s8 |

      % F
      fs,4 fs16 gs b8~4 s2 |
      s1. |
      <a, ds' a>4\arpeggio a'\arpeggio s2 |
      s1. |
      s2. |
      s2. |
      s4 \tuplet 3/2 { ds,,16 cs' \change Staff="up" cs' } \change Staff="down" fs,8~8. <c' a>16 c,4 a |
      s2. \tuplet 3/2 { c'4 b a~ } |
      4~8 e'16 ds ~ ds4.. fs16 |
      a,2. cs4 |
      e2 fs~ |
      2 r8. cs'16 b8. a16 |
      gs4.. gs16~4.. gs,16~ |
      2 \tuplet 3/2 { a8 c a~ } 4 |
      gs2~4. gs16 ds' |

      % G
      \tuplet 3/2 { \change Staff="up" e'8 b \change Staff="down" e,,16 e'~ } 4 ds,16 e cs' ds s4 |
      ds,8 cs~8 gs~8 cs~16 e,8. |
      a4 s2 |
      \clef "bass" e4. a8 s2 |
      <d, fs,>4-> <cs e,>-> <d a d,>2-> |
      \clef "treble" \tuplet 3/2 { r8 a''' ds, } s2 a4~ |
      4 e'2. |
      R1
      r2 b4.. ds16~ |
      4.. b16~4.. cs16 |

      % H
      \tuplet 3/2 4 { e8 gs, cs ds gs, b } r2. |
      gs,8. s16 fs'4 s1 |
      \clef "bass" gs,2 <gs fs>4 <e' ds>~ |
      4 s2 |
      \stemDown a,,2 \mergeDifferentlyHeadedOn f \mergeDifferentlyHeadedOff |
      d4_> \stemNeutral \tupletDown \tuplet 3/2 { d'8 f'4 } \tupletNeutral r2 |
      \clef "treble" gs,16 cs ds gs, \tuplet 3/2 { c8 fs4~ } 4 |
      \stemDown gs,4 \stemNeutral s2 fs'16 c' gs a s4 |
      r4 ds, cs'2 |
      cs4 e2. |
      R1
      r2 s |

      % I
      r8. a,16 e'4 s4 r2 |
      s4 r s2 \tuplet 6/4 { e,16[ fs b r cs gs] } |
      \tuplet 6/4 4 { cs,16[ fs ds' r b fs] r[ e fs r fs gs] r[ b cs r cs ds] r[ ds cs r ds, b'] } s4 |
      gs,16 e' \change Staff="up" cs' \change Staff="down" b, s2. \clef "bass" \tuplet 6/4 { r16[ ds, e b' b, cs] } |
      a,4 fs''2 4~ \tuplet 3/2 { 4 a8 } |
      \tuplet 3/2 { fs8 cs' a } s2 \clef "treble" fs'16 ds gs b, s4 |
      s4 \clef "bass" s s8 \clef "treble" s \stemDown fs'8 \tuplet 3/2 { cs cs,16_~ } \stemNeutral |
      16 gs' b8 gs16 ds'' gs, e~ \tuplet 3/2 { 8 cs' fs, } cs4 |
      ds8 \tuplet 3/2 { cs'16 \change Staff="up" gs' fs' \change Staff="down" } \tuplet 3/2 { gs,,8 b16 ds a8 } \stemDown fs4 \stemNeutral ds'16 e8. |
      R1

      R1\fermata

      % J
      b2 e4 |
      ds2. |
      fs4 gs16 a8.~4 |
      a2 gs4~ |
      4 a2 |
      b,2. |
      e4 ds2 |
      fs2 gs4~ |
      2 a4 |
      gs2. a2 |
      b,2 e4 |
      ds2. |
      fs4 gs2. |
      a4 gs16 a8.~4 a16 gs8.\laissezVibrer \bar "|."
    }
  >>

  \layout {
    \numericTimeSignature
    \context {
      \Score
      rehearsalMarkFormatter = #format-mark-alphabet
      \override TupletBracket.bracket-visibility = #'if-no-beam
    }
    \context {
      \PianoStaff
      \consists "Span_stem_engraver"
    }
  }
  \midi {
    \tempo 4=112
  }
}

\pointAndClickOff

\paper {
  #(set-paper-size "letter")
  indent = 0
  left-margin = 0.5\in
  top-margin = 0.5\in
  right-margin = 0.5\in
  bottom-margin = 0.5\in

  max-systems-per-page = 6
  % page-count = 10
  % systems-per-page = 5

  % Based on
  % https://github.com/lilypond/lilypond/blob/master/ly/titling-init.ly
  bookTitleMarkup = \markup {
    \override #'(baseline-skip . 3.5)
    \override #'(font-name . "Optima nova LT Pro")
    \column {
      \abs-fontsize #20
      \fill-line { \fromproperty #'header:title }
      \vspace #0.5
      \fill-line {
        \null
        \abs-fontsize #12
        \fromproperty #'header:composer
      }
    }
  }

  evenHeaderMarkup = \markup {
    \abs-fontsize #10
    \column {
      \fill-line {
        \null
        \if \should-print-page-number \fromproperty #'page:page-number-string
      }
      \unless \on-first-page-of-part \vspace #0.5
    }
  }

  oddHeaderMarkup = \evenHeaderMarkup
}

\header {
  tagline = ##f
}
