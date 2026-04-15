\version "2.25.23"

\header {
  title = "Falling Trees"
  composer = "Nathan Whetsell"
}

\language "english"

\score {
  \new PianoStaff <<
    \new Staff="up" \relative {
      \set beamMinimumSubdivision = #1/8
      \accidentalStyle piano
      \key g \minor

      \tempo \markup {
        "Supple"
        \normal-text \concat { "(" \smaller { \general-align #Y #DOWN \note {4} #0.75 } " = c. 112)" }
      }
      \time 4/4
      \ottava #1 c''''8^"With lots of pedal" \tuplet 3/2 { df16 bf df } ef8. \set subdivideBeams = ##t bf16 16 g \tuplet 3/2 { e' a, g } \unset subdivideBeams e'8. df,16 |
      \tuplet 6/4 4 { df'16 bf fs c ef a, df fs g ef g a bf, df g bf, df fs bf, e bf g a ef' } |
      \time 3/4 \tuplet 6/4 { ef,16 fs' bf, ef, df' ef } \stemDown \tuplet 6/4 { \crossStaff { ef16 } s16*3 \crossStaff { e16 } s } \tuplet 3/2 { \crossStaff { <e e,>8 } s \crossStaff { <ef gf,> } } \stemNeutral |
      \time 2/4 <df bf>4-> \ottava #0 \set subdivideBeams = ##t ef,16 bf' ef,32 e g a \unset subdivideBeams |
      \time 3/4 \grace s8 <bf bf,>4-^ ef, \acciaccatura g8 ef8 a |
      \time 4/4 g16 ef \change Staff="down" g,,8 \change Staff="up" c'16 bf df \change Staff="down" e, \change Staff="up" \once \override TupletNumber.stencil = ##f \tuplet 6/4 { s8. \crossStaff { df'16 } s8 } \change Staff="down" e,,8[ \change Staff="up" c''] |
      bf8. g16 g4 a2 |
      R1
      r2 r4 \ottava 1 g''4 |
      fs16 g df' \change Staff="down" df, \change Staff="up" \tuplet 6/4 { c' \change Staff="down" df, e df \change Staff="up" bf e, } \tuplet 3/2 { \change Staff="down" c bf' \change Staff="up" c' } c8 \ottava 0 r4 |

      \time 3/4 \tweak Y-offset #5.5 \ottava 2 \tweak bracket-visibility ##t \tweak direction #UP \tuplet 6/4 { g'8 bf,16
      \change Staff="down"
      % Position “loco” markup inside the staff, based on
      % https://lists.gnu.org/archive/html/lilypond-user/2018-02/msg00453.html
      \once \override TextScript.outside-staff-priority = ##f
      \once \override TextScript.X-offset = #2
      \once \override TextScript.Y-offset = #3
      c,8_\markup { \italic "loco" }
      \change Staff="up" fs'16 } \tweak bracket-visibility ##t \tweak direction #UP \tuplet 3/2 { df8 bf16 } c8\laissezVibrer \ottava 0 r4 |

      \time 2/4 \tweak shorten-pair #'(0 . 0.1) \ottava 2 \tuplet 6/4 { bf'16 df, bf bf' bf, \ottava 0 \change Staff="down" e,16 \change Staff="up" } r4 |
      \time 4/4 \ottava 2 c''16 e, e bf' c2. \ottava 0 |
      R1\fermata \bar "||"

      % A
      \mark \default
      \tempo \markup { \normal-text \concat { \smaller { \general-align #Y #DOWN \note {4} #0.75 } " = c. 78" } }
      \time 3/4
      \ottava 2 <c bf e, df>4-> e,8 a ef-> <df a> |
      <g g,>8.-> <e fs,>16\laissezVibrer r8 a,-> bf4 |
      \time 4/4
      \ottava 1 <a bf,>4.-> df,8 <g c,>4 <c, g fs> |
      <fs df c a>4 <e df bf>8. <ef df>8. <a, g ff ef>8. <fs bf,>8. |
      <e' e,>4 <ef ef,>8. <a, g c, bf>16\laissezVibrer \ottava 0 r8 <bf ef, df> <fs bf,> <e c> |
      <ef df>8.[ <fs c>8. <g e df bf>8.\laissezVibrer r8.] \clef "bass" <e, df bf g e>4 \clef "treble" |
      \set PianoStaff.connectArpeggios = ##t
      \override PianoStaff.Arpeggio.arpeggio-direction = #UP
      <g' fs e bf>8.\arpeggio[ ef8. <ef g,>8\laissezVibrer] \tuplet 3/2 4 { r8[ df-> <bf ef,>->] <a g ef>-> c-> df-> } |
      \time 3/4 <df bf a>8-> df c \acciaccatura df8 e8 g <gf df c> |
      \time 4/4
      <bf a fs>8.[ bf8. <c bf g>8.\laissezVibrer r8.] r4 |
      bf4 <ef df bf>2. |
      <df c>1 |
      r2 r4 \ottava 1 df'\laissezVibrer \ottava 0 |
      \time 3/4 r4 \ottava 1 bf8\laissezVibrer \ottava 0 \change Staff="down" g,\laissezVibrer \change Staff="up" r4 | \bar "||"

      % B
      \mark \default
      \tempo \markup { \normal-text \concat { \smaller { \general-align #Y #DOWN \note {4} #0.75 } " = c. 104" } }
      \time 4/4
      \ottava 2 c''8 bf \stemDown \change Staff="down" g,16 \change Staff="up" e'32 \change Staff="down" bf32 g8\laissezVibrer \change Staff="up" r8 \tuplet 3/2 { a'16 \change Staff="down" bf, g \change Staff="up" } \stemNeutral e' ef8. |
      g16 ef df8\laissezVibrer r[ e16 \change Staff="down" a,] \change Staff="up" \ottava #0 s2 |
      \time 5/4
      \tuplet 3/2 { bf8 df, bf\laissezVibrer } r4 df16 c bf a c a fs \change Staff="down" bf, \change Staff="up" e' df a8 |
      \tuplet 6/4 4 {
        df16 bf e, df \change Staff="down" g, df \change Staff="up"
        e''16 g, ef df \change Staff="down" bf e, \change Staff="up"
        ef''16^\markup { \italic "molto ritard." } df a g \change Staff="down" df \change Staff="up" a'
        g16 \change Staff="down" e ef df a \change Staff="up" e'
      } \tuplet 3/2 { bf8 \change Staff="down" e, df \change Staff="up" } |
      \tempo \markup { "a tempo" \normal-text \concat { "(" \smaller { \general-align #Y #DOWN \note {4} #0.75 } " = c. 104)" } }
      <e'' e,>4 e16 \change Staff="down" bf,8.\laissezVibrer \change Staff="up" \tuplet 3/2 { e'8 df ef, } <a df,>16 g c, \change Staff="down" g, \change Staff="up" ef'' df gf, \change Staff="down" df \change Staff="up" |
      \time 2/4 \set subdivideBeams = ##t bf'16 gf' \tweak bracket-visibility ##t \tuplet 3/2 { ef16 \change Staff="down" e, a, \change Staff="up" } \unset subdivideBeams \tuplet 3/2 { \change Staff="down" fs \change Staff="up" gf'' df\laissezVibrer } r8 |
      \time 3/8 \tuplet 6/4 { g16 e df bf a' e\laissezVibrer } r8 |
      \time 3/4 \change Staff="down" df,,,8 \change Staff="up" g'''16 c,\laissezVibrer s4 \change Staff="down" e,,16 a, \change Staff="up" bf'' \change Staff="down" a,,, \change Staff="up" |
      \time 3/8 \tuplet 3/2 { g''''8 e ef\laissezVibrer } r8 |
      \time 3/4
      ef8 g,16 \change Staff="down" g,16 \change Staff="up" \tuplet 3/2 { a'8 g \change Staff="down" ef \change Staff="up" } df'16 c df \change Staff="down" df, \change Staff="up" |
      \change Staff="down" c,16 \change Staff="up" e'' bf df c8[ \tuplet 3/2 { r16 df e] } \tuplet 3/2 { \change Staff="down" fs, \change Staff="up" bf g' } \change Staff="down" g,,8 \change Staff="up" |
      \time 4/4
      \change Staff="down" \tweak bracket-visibility ##t \tweak TupletBracket.padding #2 \tuplet 6/4 { bf16 a8 \change Staff="up" gf''16 df \change Staff="down" bf, \change Staff="up" } bf'' a gf \change Staff="down" g, df \change Staff="up" bf'' e, ef\laissezVibrer s4 |
      c'16 bf8 \change Staff="down" c,16 s4 \tuplet 3/2 { e,8 \change Staff="up" df'' bf } e, \change Staff="down" g,, \change Staff="up" |
      s4 r r c''16 ef,8. |
      \change Staff="down" ef16 bf \change Staff="up" df' c\laissezVibrer r4 r2 |
      \time 3/4
      r4 \tupletUp \tweak bracket-visibility ##t \tweak TupletBracket.padding #1.9 \tuplet 3/2 { \change Staff="down" c,8 \change Staff="up" df' \change Staff="down" fs, } \tweak bracket-visibility ##t \tweak TupletBracket.padding #0.9 \tuplet 3/2 { \change Staff="up" g' \change Staff="down" g,4\fermata } \tupletNeutral \change Staff="up" | \bar "||"

      % C
      \mark \default
      a'16 c, \change Staff="down" g8 \change Staff="up" df'16 bf a g <df' fs,> bf ef, \change Staff="down" ef, \change Staff="up" |
      \time 4/4 ef''16 c bf g e \change Staff="down" c a g \change Staff="up" \tuplet 6/4 { bf'16 g fs e \change Staff="down" ef c \change Staff="up" } \tuplet 6/4 { bf'16 g fs e \change Staff="down" df bf \change Staff="up" } |
      \time 3/4
      g'16 gf ef \change Staff="down" fs, \change Staff="up" bf' e, bf \change Staff="down" g \change Staff="up" df' \change Staff="down" bf, \change Staff="up" df'' bf |
      \tempo \markup { \normal-text { \italic "poco ritard." } }
      \tuplet 3/2 4 { g8 \change Staff="down" fs, df \change Staff="up" df' \change Staff="down" g, e \change Staff="up" df' bf \change Staff="down" a \change Staff="up" } |
      g'16 df c \change Staff="down" ef, s4 \stemUp \tuplet 6/4 { bf16 g' \change Staff="up" ef' \change Staff="down" bf, \change Staff="up" g'' \change Staff="down" e, \change Staff="up" } \stemNeutral |
      \time 4/4
      \tuplet 6/4 4 { \change Staff="down" g,16 e ef \change Staff="up" e'' ef df \change Staff="down" a,, \change Staff="up" g'' \change Staff="down" bf, df, \change Staff="up" a'' e } \tempo \markup { \normal-text \concat { \smaller { \general-align #Y #DOWN \note {4} #0.75 } " = c. 96" } } c4 bf'16 e, df \change Staff="down" a, \change Staff="up" | \bar "||"
      \tempo \markup { \normal-text { \italic "molto ritard." } }
      \key e \major
      cs''!16 a fs \change Staff="down" cs \change Staff="up" e' \change Staff="down" cs,! c gs c, \change Staff="up" e' fs c' \tuplet 6/4 { e <ds! a> ds a \change Staff="down" c, a \change Staff="up" } |
      \tempo \markup { \normal-text \concat { \smaller { \general-align #Y #DOWN \note {4} #0.75 } " = c. 104" } }
      a''16 gs \! e \change Staff="down" a, \change Staff="up" fs' e gs, \change Staff="down" c, \change Staff="up" a''32 cs a8. <a c,>4 |
      a4 s s \tuplet 3/2 { gs'8 fs ds } |
      \time 5/4 gs8 cs, a' e \tuplet 3/2 { gs8 a, gs } \tempo \markup { \normal-text { \italic "ritard." } } gs'16 c, a gs' ds a8. |
      \time 4/4
      \tempo \markup { \normal-text \concat { \smaller { \general-align #Y #DOWN \note {4} #0.75 } " = c. 72" } }
      \tweak shorten-pair #'(0 . 0.1) \ottava 1 \tuplet 3/2 { c'16 a gs } \ottava 0 \change Staff="down" e,8\laissezVibrer \change Staff="up" r4 \change Staff="down" \acciaccatura cs'8 fs,[ \change Staff="up" \tuplet 3/2 { r16 \ottava 1 a' gs]\laissezVibrer] \ottava 0 } r4 | \bar "||"

      % D
      \mark \default
      \ottava 1 a8 4 8\laissezVibrer \ottava 0 r2 |
      r2 r4 \ottava 1 cs16 e,8.\laissezVibrer \ottava 0 |
      \time 3/2 s16^\markup { \italic "accel." } s8. r4 \tuplet 3/2 { gs,,8 c gs'\laissezVibrer } r4 \tuplet 3/2 { gs,8 a cs4 fs, 8\change Staff="down" gs, \change Staff="up" } |
      \time 4/4
      \tempo \markup { \normal-text \concat { \smaller { \general-align #Y #DOWN \note {4} #0.75 } " = c. 104" } }
      cs'16 fs, \change Staff="down" gs, \change Staff="up" cs' c gs e a e'8 cs16 \change Staff="down" a, \change Staff="up" a' fs ds \change Staff="down" gs, |
      \tuplet 6/4 4 { e16 \change Staff="up" a' gs \change Staff="down" ds, cs' c \change Staff="up" cs' gs e e'16 ds \change Staff="down" fs, } fs, \change Staff="up" c'' cs, \change Staff="down" cs, ds \change Staff="up" c' \change Staff="down" a,8 \change Staff="up" |
      \time 3/4
      ds''16 cs gs \change Staff="down" e, \change Staff="up" \tuplet 3/2 { a'8 e \change Staff="down" ds, \change Staff="up" } s4 |
      <ds''! c cs,!>4->^\markup { \italic "accel." } \tuplet 3/2 { \tweak Y-offset #6 \ottava 2 a'''8-^ \ottava 0 \change Staff="down" a,, gs \change Staff="up" } s4 |
      r4 \tuplet 3/2 { a,8 e \change Staff="down" cs \change Staff="up" } cs'16 \change Staff="down" a \change Staff="up" e' \change Staff="down" ds, \change Staff="up" |
      \tempo \markup { \normal-text \concat { \smaller { \general-align #Y #DOWN \note {4} #0.75 } " = c. 144" } }
      \tuplet 3/2 4 { ds'8 \change Staff="down" a fs \change Staff="up" cs' \change Staff="down" a c, \change Staff="up" } gs''16 cs, a' e |
      \tempo \markup { \normal-text { \italic "ritard." } }
      gs16 e ds \change Staff="down" c \change Staff="up" \tuplet 3/2 { a'8 gs gs } a \change Staff="down" gs, \change Staff="up" |
      \time 4/4
      cs'8 a ds4 \tuplet 3/2 { e4 ds8 } \tuplet 3/2 { fs4 e8 } |
      \tempo \markup { \normal-text \concat { \smaller { \general-align #Y #DOWN \note {4} #0.75 } " = c. 129" } }
      gs16 cs, \change Staff="down" e,16 a, gs e' \change Staff="up" gs \change Staff="down" c,\laissezVibrer \change Staff="up" r8[ a'16 gs] \change Staff="down" a,8 \change Staff="up" gs''16 gs, |
      \time 3/4
      \tempo \markup { \normal-text { \italic "molto ritard." } }
      r4 \ottava 1 \tuplet 3/2 { gs'8 c a } \tupletUp \tweak TupletBracket.padding #2 \tuplet 3/2 { c,8 \ottava 0 \change Staff="down" ds,4 \change Staff="up" } \tupletNeutral |
      \ottava 1 a''16 \ottava 0 \change Staff="down" gs, gs,8\laissezVibrer \change Staff="up" r4 r |
      \tempo \markup { \normal-text \concat { \smaller { \general-align #Y #DOWN \note {4} #0.75 } " = c. 72" } }
      \ottava 1 a''16 a,8. a'16 b,8.\laissezVibrer r4 |
      gs'16 a,8. c'16 c,8.\laissezVibrer r4 |
      \time 4/4
      a'16 b,8. cs16 gs8.\laissezVibrer r8 gs16 a\laissezVibrer r4 |
      fs8. \change Staff="down" a,16_\markup { \italic "loco" } \change Staff="up" a'8 gs'\laissezVibrer r c, s8. \ottava 0 s16 |
      \tempo \markup { \normal-text { \italic "poco accel." } }
      a4 \change Staff="down" gs,16 cs \change Staff="up" a'8\laissezVibrer r2 | \bar "||"

      % E
      \mark \default
      \tempo \markup { \normal-text \concat { \smaller { \general-align #Y #DOWN \note {4} #0.75 } " = c. 156" } }
      \time 3/4 r4 e'8 a, cs4 |
      \time 4/4
      \tuplet 3/2 { \change Staff="down" e,,8 b' \change Staff="up" cs' } cs4 \acciaccatura { \slurUp cs,8 } a'4 \slurNeutral \tempo \markup { \normal-text { \italic "ritard." } } \tuplet 3/2 { b,8 fs cs } |
      \tempo \markup { \normal-text \concat { \smaller { \general-align #Y #DOWN \note {4} #0.75 } " = c. 104" } }
      \change Staff="down" a,16 ds \change Staff="up" e' \change Staff="down" a,,, \change Staff="up" \tuplet 3/2 { ds'''8 e, \change Staff="down" a,, \change Staff="up" } a''16 cs, cs' \change Staff="down" gs,, \change Staff="up" s4 |
      \tempo \markup { \normal-text { \italic "poco più mosso" \concat { "(" \smaller { \general-align #Y #DOWN \note {4} #0.75 } " = c. 120)" } } }
      a''16 \change Staff="down" gs, cs a \change Staff="up" e'' ds a \change Staff="down" gs, \change Staff="up" s4 a'16 \change Staff="down" cs, \change Staff="up" gs'' ds |
      \time 3/4 \change Staff="down" a,16 gs \change Staff="up" gs'' a \tuplet 3/2 { a,8 fs' e } s4 |
      \tempo \markup { \normal-text { \italic "ritard." } }
      \time 4/4 cs'8 \change Staff="down" a, \change Staff="up" a' \change Staff="down" cs, \tuplet 3/2 { e, gs \change Staff="up" b' } \stemUp \once \override Beam.positions = #'(8 . 7) e16 a, cs ds, |
      \time 2/4 \once \override Beam.positions = #'(8 . 8) b'16 \change Staff="down" <e, fs,> \change Staff="up" fs' \change Staff="down" ds, \change Staff="up" \once \override Beam.positions = #'(8.5 . 7) gs' e ds \change Staff="down" cs, \change Staff="up" \stemNeutral \revert Beam.positions |
      \tempo \markup { \normal-text \concat { \smaller { \general-align #Y #DOWN \note {4} #0.75 } " = c. 104" } }
      \time 4/4
      \tuplet 7/4 { \change Staff="down" a,16 \change Staff="up" gs''' \change Staff="down" a,, gs \change Staff="up" ds'' a' \change Staff="down" cs,, } \tuplet 3/2 4 { fs,8 \change Staff="up" a' \change Staff="down" ds, \change Staff="up" r8 a'4 gs'8 gs, \change Staff="down" e \change Staff="up" } |
      r4 \ottava 1 ds''16 cs gs8\laissezVibrer \ottava 0 r4 \tuplet 3/2 { a4 fs8\laissezVibrer } |
      r2 gs16 \change Staff="down" a,8 \change Staff="up" b'16 a8 b16 fs |
      r2 r4 r8. ds16 |
      cs2\laissezVibrer r8. e16 r4 |
      r4 fs4 ds2\laissezVibrer |
      r2 r4 r8 \tuplet 3/2 { \change Staff="down" cs,16 ds \change Staff="up" \ottava 1 cs'' \ottava 0 } | \bar "||"

      % F
      \mark \default
      \time 5/4 s2. \change Staff="down" a,,16 ds \change Staff="up" ds'8\laissezVibrer r4 |
      \tempo \markup { \normal-text { \italic "accel." } }
      \time 3/2 \change Staff="down" gs,,,16 \change Staff="up" e'' gs8\laissezVibrer r4 \change Staff="down" b,,16 \change Staff="up" ds'16 ds'8\laissezVibrer r4 \tuplet 3/2 { \change Staff="down" gs,,,16 gs' \change Staff="up" cs' } b8\laissezVibrer r16 gs8. |
      \tempo \markup { \normal-text \concat { \smaller { \general-align #Y #DOWN \note {4} #0.75 } " = c. 155" } }
      \time 4/4 cs4\arpeggio \change Staff="down" a,16 \change Staff="up" gs'8. \tuplet 3/2 4 { \change Staff="down" cs,,8 \change Staff="up" b'' gs a \change Staff="down" a,, b \change Staff="up" } |
      \tempo \markup { \normal-text { \italic "ritard." } }
      \tuplet 3/2 4 { cs'8 gs' \change Staff="down" b,,8 a \change Staff="up" ds' cs] } \change Staff="down" gs, ds' e \change Staff="up" a |
      \time 2/4 \tuplet 3/2 4 { ds fs, b e gs, \change Staff="down" a, \change Staff="up" } |
      \tempo \markup { \normal-text \concat { \smaller { \general-align #Y #DOWN \note {4} #0.75 } " = c. 104" } }
      \time 3/4
      \change Staff="down" ds16 \change Staff="up" <fs' ds> e \change Staff="down" gs,, ds' \change Staff="up" cs' e \change Staff="down" a, \change Staff="up" \acciaccatura { \slurUp ds8 } ds'8 \slurNeutral \change Staff="down" cs, \change Staff="up" |
      \tempo \markup { \normal-text { \italic "accel." } }
      \change Staff="down" cs,16 fs, \change Staff="up" e'' ds \tuplet 3/2 { \change Staff="down" gs,8 \change Staff="up" a' gs } a,16 \change Staff="down" c, e, \change Staff="up" ds'' |
      \time 5/4
      \tempo \markup { \normal-text { \italic "ritard." } }
      gs16 \change Staff="down" e, \change Staff="up" a' \change Staff="down" e, \change Staff="up" s4 \set subdivideBeams = ##t \tuplet 3/2 { e''16 gs, cs, } ds'16 \change Staff="down" <c, a> \change Staff="up" \unset subdivideBeams e' cs b gs\laissezVibrer r4 |
      gs'16 cs, \change Staff="down" e, a, fs \change Staff="up" a' fs \change Staff="down" a, \change Staff="up" gs' e \change Staff="down" c a \change Staff="up" e''4 e, |
      \tempo \markup { \normal-text \concat { \smaller { \general-align #Y #DOWN \note {4} #0.75 } " = c. 104" } }
      \time 4/4
      r8 \ottava 1 gs'16 e a fs \ottava 0 \change Staff="down" e, ds \change Staff="up" r2 |
      r8. gs'16 fs4\laissezVibrer r8. cs16 r8. a'16\laissezVibrer |
      R1
      b1 |
      r2 cs, |
      e2\laissezVibrer r4 cs |
      \tempo \markup { \normal-text { \italic "ritard." } }
      \change Staff="down" gs,8 \change Staff="up" fs'\laissezVibrer r4 r8 cs'8 gs' \change Staff="down" gs,,16 ds' \change Staff="up" | \bar "||"

      % G
      \tweak X-offset #6 \mark \default
      \once \override Score.MetronomeMark.Y-offset = 7 \tempo \markup { \normal-text \concat { \smaller { \general-align #Y #DOWN \note {4} #0.75 } " = c. 78" } }
      s4 \tupletUp \tuplet 3/2 { \once \override NoteColumn.X-offset = 0.75 ds'8 a4 } \tupletNeutral s4 c16 e c, \change Staff="down" e,, \change Staff="up" |
      \tempo \markup { \normal-text \concat { \smaller { \general-align #Y #DOWN \note {4} #0.75 } " = c. 104" } }
      r16 e'( gs fs ds'[ fs) r a,]( cs e a gs ds) \change Staff="down" e,, \change Staff="up" cs'' e |
      \time 3/4 fs4 \set subdivideBeams = ##t \tuplet 3/2 { \change Staff="down" ds,16 \change Staff="up" ds' gs } a \change Staff="down" e, \unset subdivideBeams fs \change Staff="up" fs' a, gs' |
      \time 4/4
      \tempo \markup { \normal-text { \italic "ritard." } }
      r16 e,( gs fs cs' e) \change Staff="down" a,, \change Staff="up" gs' \change Staff="down" fs, cs' \change Staff="up" a' fs'-> \change Staff="down" cs,, a' \change Staff="up" e'16 d'-> |
      \tempo \markup { \normal-text \concat { \smaller { \general-align #Y #DOWN \note {4} #0.75 } " = c. 78" } }
      r16 cs, e c'-> r gs, c a' \tempo \markup { \normal-text { \italic "accel." } } \tuplet 3/2 4 { <cs fs,>8-> fs,, gs cs' a <gs e>\laissezVibrer } |
      \tempo \markup { \normal-text \concat { \smaller { \general-align #Y #DOWN \note {4} #0.75 } " = c. 104" } }
      r4 \change Staff="down" e'8 \change Staff="up" \ottava 1 \tuplet 3/2 { c''16 gs \ottava 0 \change Staff="down" a, } a,8 \change Staff="up" \tuplet 3/2 { \ottava 1 ds''16 c e,\laissezVibrer \ottava 0 } r4 |
      r8. \ottava 1 e16 \ottava 0 r4 r8 \ottava 1 a~ \tuplet 3/2 { 8 ds, a~ } |
      \tuplet 3/2 { 8 cs c~ } 4 cs2 |
      fs16 c8.\laissezVibrer r8 gs'16 gs, \ottava 0 r2 |
      \tempo \markup { \normal-text { \italic "molto ritard." } }
      e'4\laissezVibrer r a,\laissezVibrer r | \bar "||"

      % H
      \mark \default
      \time 5/4 r2 fs8. gs,16 \tempo \markup { \normal-text \concat { \smaller { \general-align #Y #DOWN \note {4} #0.75 } " = c. 46" } } a'8 a16 a, <a' fs>4 |
      \tempo \markup { \normal-text { \italic "accel." } }
      \time 6/4 b,16 e'8. cs16 gs a cs, \change Staff="down" gs,16 ds' \change Staff="up" fs' gs \change Staff="down" cs,, \change Staff="up" e' a cs \change Staff="down" a,, gs' a \change Staff="up" b' ds, \change Staff="down" cs, ds e \change Staff="up" |
      \tempo \markup { \normal-text \concat { \smaller { \general-align #Y #DOWN \note {4} #0.75 } " = c. 104" } }
      \time 4/4 cs'16 e a, gs \set subdivideBeams = ##t \tuplet 3/2 { cs16 a e' } \unset subdivideBeams fs,32 gs a ds32 4 e |
      \time 3/4 ds16 cs e fs ds \change Staff="down" ds, e a, cs, \change Staff="up" gs'' e' e, |
      \time 4/4
      \tuplet 3/2 4 { ds8 e cs a' c gs } \change Staff="down" f,,16 c' \change Staff="up" c' e cs' \change Staff="down" e,, a, \change Staff="up" cs' |
      f'16-> e, \change Staff="down" c \change Staff="up" a' \tuplet 3/2 { e'8-> f-> gs,-> } a'4 \tuplet 3/2 { a,8 c e, } |
      \time 3/4 gs4 \tuplet 3/2 4 { fs'8 gs <e c> <fs ds>-> <gs c,>-> <a ds,>-> } |
      \time 5/4 ds,8. a16 \change Staff="down" c, cs \change Staff="up" cs' gs' \change Staff="down" ds, e gs \change Staff="up" a' s4 \tuplet 6/4 { \change Staff="down" cs,16 \change Staff="up" ds a' c \change Staff="down" a, \change Staff="up" ds } |
      \time 4/4
      \tuplet 3/2 { cs'8 e,16 } c'8\laissezVibrer r8. e16 r4 r16 ds,8 ds16 |
      r8. ds'16 r8. cs16 a4 r8. cs16 |
      \tempo \markup { \normal-text { \italic "ritard." } }
      c2 r8. a16 fs'8. gs,16 |
      \tempo \markup { \normal-text \concat { \smaller { \general-align #Y #DOWN \note {4} #0.75 } " = c. 84" } }
      gs'4 r8. gs,16 e'8 \change Staff="down" cs, \tuplet 3/2 { gs16 c ds } \change Staff="up" a'8\laissezVibrer | \bar "||"

      % I
      \mark \default
      \time 5/4
      r2 \tuplet 3/2 { \change Staff="down" gs,16 ds' \change Staff="up" ds' } ds8 ds4 fs, |
      \tempo \markup { \normal-text { \italic "accel." } }
      \tuplet 3/2 { e'8 \change Staff="down" a,, \change Staff="up" a' } fs'16 fs cs8 \change Staff="down" gs,16 \change Staff="up" fs' ds' e, \tuplet 3/2 { ds'8 \change Staff="down" ds, e, \change Staff="up" } r gs' |
      \tempo \markup { \normal-text { \italic "ritard." } }
      <<
        { \stemDown ds'8([ cs ds e] cs[ e b gs] \stemUp fs4) }
        \\
        { s1 \once \override Beam.positions = #'(-6 . -5) \tuplet 6/4 { fs16 \change Staff="down" \stemUp cs b e, \stemNeutral \change Staff="up" cs' cs' } }
      >> |
      \tempo \markup { \normal-text \concat { \smaller { \general-align #Y #DOWN \note {4} #0.75 } " = c. 75" } }

      <<
        { gs8 cs, }
        \\
        { \once \override Beam.positions = #'(6 . 6) \change Staff="down" \stemUp gs,16 e' \change Staff="up" \stemDown cs' \change Staff="down" \stemUp b, \change Staff="up" }
      >> \tuplet 3/2 { b''8 e, \change Staff="down" gs,, } e16 ds' \change Staff="up" e' cs \tuplet 6/4 { a' b, gs' \change Staff="down" fs, \change Staff="up" fs' ds } \tuplet 3/2 { gs8 ds b } |

      \tempo \markup { \normal-text { \italic "accel. poco a poco" } }
      cs4 r2 r |
      r4 a8 \change Staff="down" fs, \change Staff="up" b'16 \change Staff="down" gs, e \change Staff="up" ds''\laissezVibrer s4 e16 \change Staff="down" a, \change Staff="up" gs' \change Staff="down" gs, \change Staff="up" |
      \time 4/4
      \change Staff="down" b16 \change Staff="up" b' e, e, \tuplet 6/4 4 { \change Staff="down" e,16 a ds gs, \change Staff="up" fs' gs gs' e ds \change Staff="down" a, b ds \change Staff="up" } \tuplet 3/2 { \change Staff="down" fs \change Staff="up" cs' gs } gs'8 |
      \tempo \markup { \normal-text \concat { \smaller { \general-align #Y #DOWN \note {4} #0.75 } " = c. 104" } }
      ds'8( e ds a cs e b8. gs16 |
      fs4) s \set subdivideBeams = ##t \tuplet 3/2 { a8 fs16 gs' \change Staff="down" cs,,,16 cs' \change Staff="up" } r16[ cs' ds32 e ds gs] \unset subdivideBeams |
      a1 |

      R1\fermata \bar "||"

      % J
      \mark \default
      \time 3/4
      r16^"l.v. sempre" gs8. r4 r16 gs8. |
      r16 ds8. r4 r |
      r16 b8. r4 r |
      r16 e8. r4 r8 gs |
      r4 r16 gs8. r4 |
      r16 ds8. r4 r |
      r16 b8. r16 a8. r4 |
      r16 e'8. r4 r8 gs8 |
      r4 r4 r16 gs8. |
      \time 5/4 r16 ds8. r4 r r16 b8. r4 |
      \time 3/4
      r16 a8. r4 r16 e'8. |
      r8 gs8 r4 r |
      \time 4/4
      r16 gs8. r16 ds8. r2 |
      r16 b8. r4 r2 \bar "|."
    }

    \new Dynamics {
      s1 \pp
      \override DynamicTextSpanner.style = #'none
      s1 \cresc
      s2.
      s2 \f \<
      s2. \ff \>
      s4 \tweak X-offset #-3.5 \mf s2. \tweak Y-offset #-1 \dim
      s1 \p \>
      s1 \pp
      s1*2 \cresc
      s2 \tweak X-offset #-2 \tweak Y-offset #-2 \mf s4 \dim
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
      s4 \f s s2. \cresc
      s1*7/8
      s2. \ff
      s4. \dim
      s2.*2
      s1
      s1*3 \tweak Y-offset #-6.2 \pp
      s2.

      % C
      s2. \pp
      s1
      s2.*3 \tweak Y-offset #-6 \cresc
      s2 s \ff
      s1
      s1*2 \tweak Y-offset #-4 \dim
      s1*5/4
      s1 \tweak Y-offset #-2 \pp

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
      s1

      % E
      s4 \pp s2 \cresc
      s2. s4 \f
      s1
      s2 s \dim
      s2.
      s1
      s2
      s1
      s1*6 \pp

      % F
      s1*5/4 \pp
      s1.
      s1
      s2 s \mf
      s2
      s2.
      s2. \tweak Y-offset #2 \dim
      s1*5/4*2
      s1*7 \pp

      % G
      s2 \pp s \cresc
      s1 \mf
      s2.
      s1 \cresc
      s2 s4 \ff \> \tuplet 3/2 { s s8 \mp }
      s1 \dim
      s4 s2. \pp
      s1*3

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
      r8 df' c4\laissezVibrer \tuplet 3/2 { r8[ e, a] } e'4\laissezVibrer |
      \override TextSpanner.dash-period = #1
      \override TextSpanner.dash-fraction = #0.3
      \override TextSpanner.bound-details.left.padding = #-4.1
      \override TextSpanner.bound-details.left.text = \markup { \italic { "loco " } }
      \override TextSpanner.bound-details.right.text = \markup { \draw-line #'(0 . -0.8) }
      \override TextSpanner.bound-details.right-broken.padding = #0
      \override TextSpanner.bound-details.right.padding = #-1
      \override TextSpanner.bound-details.right-broken.text = ""
      s4 \stemDown \once \override Beam.positions = #'(-3.5 . -5) \tuplet 6/4 { df16\startTextSpan \change Staff="up" g df' \change Staff="down" <bf, gf> e,16 df } \tuplet 3/2 { e8 <c' e,>16 g  bf8\stopTextSpan \stemNeutral } |
      a,8.-> a'16 e4 \clef "bass" |
      \acciaccatura e,,8 <ef c>2-^ r8 c'' |
      s2 \stemDown \tuplet 6/4 { fs16 e g fs e \change Staff="up" g \change Staff="down" } \stemNeutral s4 |
      R1
      e,1 |
      \clef "treble" df'2 bf'' |
      s8. \ottava 1 s16 s4 \tuplet 3/2 4 { s8 \ottava 0 s4 r8 r e, } |
      s4 s r8[ \tuplet 3/2 { fs16 g e] } |
      s4 \tuplet 3/2 { df'16 ef g,\laissezVibrer } r8 |
      R1

      R1\fermata

      % A
      \ottava #1 <c' bf g fs>4-> r8 <a e bf> fs4-> |
      df,4-> r8 <e' c>-> <ef df>4 \ottava #0 |
      <<
        {
          <g, fs df bf>2-> <e df>4 bf\laissezVibrer |
          r4 <g e>8.[ df'8. <a g fs>8.\laissezVibrer]
        }
        \\
        {
          r8 bf,4 \mf c df fs,8~
          8 bf4 df8\laissezVibrer
        }
      >> r8 <df df,>16 |
      <bf' g e>4 <a ef bf>8. <df, g,>16\laissezVibrer \clef "bass" r8 <ef df,>4. |
      fs,2. \ottava #-1 <df df,>4 |
      <c a,>8.\arpeggio\laissezVibrer[ \ottava #0 r8. g'8\laissezVibrer] r2 |
      r8 e8 bf' fs'8 g,8 bf |
      \clef "treble" <g' df>8. <e' df>8. a,8. c8. ef4\laissezVibrer |
      R1*2
      c1 |
      r4 s r |

      % B
      s4 \ottava #1 s2. |
      s2 g''16 bf e, a ef4 \ottava #0 |
      s2. s4 g,, |
      s4 \stemDown gf, bf \stemNeutral s2 |
      df,8 c''\laissezVibrer s16 s8. s4 s8. \clef "bass" s16 s4 |
      df,,4 s8 s |
      r r bf'16 e, |
      s4 \clef "treble" fs'16 e df \clef "bass" g, s4 |
      r8 r c |
      \stemDown c,4 \stemNeutral s2 |
      s2 \clef "treble" s4 |
      s2. df'16 c a bf |
      df4 \tuplet 3/2 { a'8 ef'8 bf } s2 |
      \change Staff="up" ef'16\laissezVibrer e,\laissezVibrer \change Staff="down" df,8\laissezVibrer r4 r4 s16 \crossStaff { e8. } |
      s4 r e'16 df8.\laissezVibrer r4 |
      r4 s2 |

      % C
      s2. |
      \stemDown c,4 s df s |
      df4 e fs |
      s2. |
      df4 \stemNeutral <c e,>16 \change Staff="up" bf' a \change Staff="down" df, \once \override TupletNumber.stencil = ##f \tuplet 6/4 { s8 \crossStaff { fs16 } s8. } |
      \clef "bass" s2 c,4 s |
      \key e \major
      \stemDown fs,4 \stemNeutral s \mergeDifferentlyDottedOn c'8. \mergeDifferentlyDottedOff cs'16 \stemDown c,4 \stemNeutral |
      \clef "treble" s4 s a'2 |
      a'16 gs ds a\laissezVibrer \change Staff="up" r8[ ds''16 cs] c a \change Staff="down" e8\laissezVibrer s4 |
      fs4 c8 ds e,4 gs ds'16 a8. |
      s4 r s gs'\laissezVibrer |

      % D
      r2 r8 gs8 a16 gs cs gs\laissezVibrer |
      r8 a16 e\laissezVibrer r8 ds'16 e,\laissezVibrer r4 e |
      e16 \change Staff="up" cs'8.\laissezVibrer \change Staff="down" s2. s4 \tuplet 3/2 { s4 \clef "bass" s8 } |
      s1 |
      s1 |
      s2 \tuplet 3/2 { c,,,8 cs c, } |
      fs8-> fs'' \tuplet 3/2 { s \clef "treble" s4 } gs\laissezVibrer |
      \clef "bass" r8 a,, \tuplet 3/2 { s \clef "treble" s s } s4 |
      s2. |
      s2. |
      cs''8 a \tuplet 3/2 { fs e cs } e ds e' a, |
      s1 |
      ds4\laissezVibrer s2 |
      s4 r r |
      R2.*2
      r2 r4 r8. gs,16 |
      s2 s4 \change Staff="up" b''8. \change Staff="down" gs,,16\laissezVibrer_\markup { \italic "loco" } |
      r4 s r \tuplet 3/2 { gs8 a4\fermata } |

      % E
      e8 gs\laissezVibrer r2 |
      s1 |
      \clef "bass" s2. a,,,4 |
      \clef "treble" s2 ds''8 cs' s4 |
      s4 s \tuplet 3/2 { \change Staff="up" gs'8 \change Staff="down" gs, ds } |
      s2. s8 \crossStaff { cs } |
      \crossStaff { a'16 } s8. s4 |
      s1 |
      ds4\laissezVibrer r4 \tuplet 3/2 { r8[ b' cs,\laissezVibrer] } r4 |
      fs1 |
      b1 |
      r2 r4 ds,4 |
      r8. cs16 r4 r2 |
      r8. cs16 gs'4\laissezVibrer r4 r8 s |

      % F
      fs,4 fs16 gs b8\laissezVibrer r4 s r |
      s4 r s r s r |
      <a, ds' a>4\arpeggio s2. |
      s1 |
      s2 |
      s2. |
      s2. |
      s4 \tuplet 3/2 { ds,16 cs' \change Staff="up" cs' } \change Staff="down" fs,8\laissezVibrer s4 c4 a |
      s2. \tuplet 3/2 { c'4 b a\laissezVibrer } |
      r4 s r r8. fs'16 |
      a,4\laissezVibrer r r cs |
      e2 fs\laissezVibrer |
      r2 r8. cs'16 b8. a16 |
      gs4\laissezVibrer r8. gs16 r4 r8. gs,16 |
      r2 \tuplet 3/2 { a8 c a\laissezVibrer } r4 |
      s1 |

      % G
      \tupletUp \tweak bracket-visibility ##t \tuplet 3/2 { \change Staff="up" e''8 b \change Staff="down" e,,16 e' } \tupletNeutral s4 ds,16 e cs' ds s4 |
      ds,8 cs4 gs8~8 cs s4 |
      a4 s2 |
      \clef "bass" e2 s |
      <d fs,>4-> <cs e,>-> <d a d,>2-> |
      \clef "treble" \tuplet 3/2 { r8[ a''' ds,] } s2 a4\laissezVibrer |
      r4 e'2. |
      R1
      r2 b4\laissezVibrer r8. ds16 |
      r4 r8. b16 r4 r8. cs16 |

      % H
      \tuplet 3/2 4 { e8 gs, cs ds gs, b } r2. |
      gs,8. b16 fs'4 s1 |
      \clef "bass" gs,2 <gs fs>4 <e' ds>\laissezVibrer |
      s2. |
      \stemDown a,,2 \mergeDifferentlyHeadedOn f \mergeDifferentlyHeadedOff |
      d4_> \stemNeutral \tupletDown \tuplet 3/2 { d'8 f'4 } \tupletNeutral r2 |
      \clef "treble" gs,16 cs ds gs, \tuplet 3/2 { c8 fs4\laissezVibrer } r4 |
      gs,16 a8. s2 fs'16 c' gs a s4 |
      r4 ds, cs'\laissezVibrer r |
      cs4 e\laissezVibrer r2 |
      R1
      r2 s |

      % I
      r8. a,16 e'4 s4 r2 |
      s4 r s2 \tuplet 3/2 { e,16 fs b8 cs16 gs } |
      \tuplet 3/2 4 { cs,16 fs ds'8 b16 fs~ 16 e fs8 fs16 gs~ 16 b cs8 cs16 ds~ 16 ds cs8 ds,16 b' } s4 |
      s1 \clef "bass" \tuplet 6/4 { r16[ ds,, e b' b, cs] } |
      a,4 fs''2 4\laissezVibrer \tuplet 3/2 { r8 r a } |
      \tuplet 3/2 { fs8 cs' a } s2 \clef "treble" fs'16 ds gs b, s4 |
      s4 \clef "bass" s s8 \clef "treble" s s4 |
      cs,16 gs' b8 gs16 ds'' gs, e~ \tuplet 3/2 { 8 cs' fs, } cs4 |
      ds8 \tuplet 3/2 { cs'16 \change Staff="up" gs' fs' \change Staff="down" } \tuplet 3/2 { gs,,8 b16 ds a8 } \stemDown fs4 \stemNeutral ds'16 e8. |
      R1

      R1\fermata

      % J
      b16 r8. r4 e16 r8. |
      ds16 r8. r4 r |
      fs16 r8. gs16 a8. r4 |
      a16 r8. r4 gs8 r |
      r4 a16 r8. r4 |
      b,16 r8. r4 r |
      e16 r8. ds16 r8. r4 |
      fs16 r8. r4 gs8 r |
      r4 r4 a16 r8. |
      gs16 r8. r4 r a16 r8. r4 |
      b,16 r8. r4 e16 r8. |
      ds8 r r4 r |
      fs16 r8. gs16 r8. r2 |
      a16 r8.  gs16 a8. r4 a16 gs8.\fermata\laissezVibrer \bar "|."
    }
  >>

  \layout {
    \numericTimeSignature
    \context {
      \Score
      rehearsalMarkFormatter = #format-mark-box-alphabet
      \override Stem.stemlet-length = #1
      \override TupletBracket.bracket-visibility = #'if-no-beam
      \override TupletBracket.span-all-note-heads = ##t
    }
    \context {
      \PianoStaff
      \consists Span_stem_engraver
    }
  }
  \midi {
    \tempo \markup { \normal-text \concat { \smaller { \general-align #Y #DOWN \note {4} #0.75 } " = c. 112" } }
  }
}

\pointAndClickOff

\paper {
  #(set-paper-size "letter")

  left-margin = 0.5\in
  top-margin = 0.5\in
  right-margin = 0.5\in
  bottom-margin = 0.5\in
  last-bottom-spacing.basic-distance = #(* 0.25 (/ in staff-space))
  last-bottom-spacing.minimum-distance = last-bottom-spacing.basic-distance

  % Based on
  % https://github.com/lilypond/lilypond/blob/master/ly/titling-init.ly
  bookTitleMarkup = \markup {
    \column {
      \fill-line {
        \override #'(baseline-skip . 2.5)
        \left-column {
          \abs-fontsize #10
          "Approximate duration 8 minutes"
          \abs-fontsize #10
          "Place grace notes before the beat"
        }

        \override #'(baseline-skip . 3)
        \override #'(font-name . "TeX Gyre Pagella")
        \override #'(font-features . ("onum" "pnum"))
        \center-column {
          \abs-fontsize #20
          \fromproperty #'header:title
          \abs-fontsize #10
          \line {
            #(strftime "%B" (localtime (current-time))) #(string-trim (strftime "%e, %Y" (localtime (current-time))))
            % "October 6, 2025"
          }
        }

        \override #'(font-name . "TeX Gyre Pagella")
        \right-column {
          \vspace #0.25
          \abs-fontsize #12
          \fromproperty #'header:composer
        }
      }

      \vspace #0.25
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

  oddFooterMarkup = \markup {
    \column {
      \abs-fontsize #6
      \override #'(font-name . "TeX Gyre Pagella")
      \override #'(font-features . ("pnum"))
      \fill-line {
        \if \on-first-page {
          \null
          "© 2022–2026 Nathan Whetsell. All rights reserved."
          \null
        }
      }
    }
  }
}

\header {
  tagline = ##f
}
