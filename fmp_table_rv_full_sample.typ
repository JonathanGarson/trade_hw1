#show figure: set block(breakable: true)
#figure( // start figure preamble
  
  kind: "tinytable",
  supplement: "Table", // end figure preamble

block[ // start block

#let nhead = 1;
#let nrow = 17;
#let ncol = 7;

  #let style-array = ( 
    // tinytable cell style after
(pairs: ((0, 0), (0, 1), (0, 2), (0, 3), (0, 4), (0, 5), (0, 6), (0, 7), (0, 8), (0, 9), (0, 10), (0, 11), (0, 12), (0, 13), (0, 14), (0, 15), (0, 16), (0, 17),), align: left,),
(pairs: ((1, 0), (1, 1), (1, 2), (1, 3), (1, 4), (1, 5), (1, 6), (1, 7), (1, 8), (1, 9), (1, 10), (1, 11), (1, 12), (1, 13), (1, 14), (1, 15), (1, 16), (1, 17), (2, 0), (2, 1), (2, 2), (2, 3), (2, 4), (2, 5), (2, 6), (2, 7), (2, 8), (2, 9), (2, 10), (2, 11), (2, 12), (2, 13), (2, 14), (2, 15), (2, 16), (2, 17), (3, 0), (3, 1), (3, 2), (3, 3), (3, 4), (3, 5), (3, 6), (3, 7), (3, 8), (3, 9), (3, 10), (3, 11), (3, 12), (3, 13), (3, 14), (3, 15), (3, 16), (3, 17), (4, 0), (4, 1), (4, 2), (4, 3), (4, 4), (4, 5), (4, 6), (4, 7), (4, 8), (4, 9), (4, 10), (4, 11), (4, 12), (4, 13), (4, 14), (4, 15), (4, 16), (4, 17), (5, 0), (5, 1), (5, 2), (5, 3), (5, 4), (5, 5), (5, 6), (5, 7), (5, 8), (5, 9), (5, 10), (5, 11), (5, 12), (5, 13), (5, 14), (5, 15), (5, 16), (5, 17), (6, 0), (6, 1), (6, 2), (6, 3), (6, 4), (6, 5), (6, 6), (6, 7), (6, 8), (6, 9), (6, 10), (6, 11), (6, 12), (6, 13), (6, 14), (6, 15), (6, 16), (6, 17),), align: center,),
  )

  // tinytable align-default-array before
  #let align-default-array = ( left, left, left, left, left, left, left, ) // tinytable align-default-array here
  #show table.cell: it => {
    if style-array.len() == 0 {
      it 
    } else {
      let tmp = it
      for style in style-array {
        let m = style.pairs.find(k => k.at(0) == it.x and k.at(1) == it.y)
        if m != none {
          if ("fontsize" in style) { tmp = text(size: style.fontsize, tmp) }
          if ("color" in style) { tmp = text(fill: style.color, tmp) }
          if ("indent" in style) { tmp = pad(left: style.indent, tmp) }
          if ("underline" in style) { tmp = underline(tmp) }
          if ("italic" in style) { tmp = emph(tmp) }
          if ("bold" in style) { tmp = strong(tmp) }
          if ("mono" in style) { tmp = math.mono(tmp) }
          if ("strikeout" in style) { tmp = strike(tmp) }
        }
      }
      tmp
    }
  }

  #align(center, [

  #table( // tinytable table start
    columns: (auto, auto, auto, auto, auto, auto, auto),
    stroke: none,
    align: (x, y) => {
      let sarray = style-array.filter(a => "align" in a)
      let sarray = sarray.filter(a => a.pairs.find(p => p.at(0) == x and p.at(1) == y) != none)
      if sarray.len() > 0 {
        sarray.last().align
      } else {
        left
      }
    },
    fill: (x, y) => {
      let sarray = style-array.filter(a => "background" in a)
      let sarray = sarray.filter(a => a.pairs.find(p => p.at(0) == x and p.at(1) == y) != none)
      if sarray.len() > 0 {
        sarray.last().background
      }
    },
 table.hline(y: 1, start: 0, end: 7, stroke: 0.05em + black),
 table.hline(y: 11, start: 0, end: 7, stroke: 0.05em + black),
 table.hline(y: 18, start: 0, end: 7, stroke: 0.1em + black),
 table.hline(y: 0, start: 0, end: 7, stroke: 0.1em + black),
    // tinytable lines before

    table.header(
      repeat: true,
[ ], [OLS (Log \+ 1)], [OLS (Log \+ 1) \- Aug.], [OLS], [OLS \- Aug.], [Poisson], [Poisson \- Aug.],
    ),

    // tinytable cell content after
[ln Distance], [\-1.751\*\*\*], [\-1.426\*\*\*], [\-1.399\*\*\*], [\-1.116\*\*\*], [\-0.857\*\*\*], [\-0.487\*\*\*],
[], [(0.064)], [(0.070)], [(0.044)], [(0.050)], [(0.043)], [(0.043)],
[Border], [0.912\*\*\*], [0.602\*\*\*], [0.737\*\*\*], [0.490\*\*\*], [0.566\*\*\*], [0.282\*\*\*],
[], [(0.165)], [(0.169)], [(0.109)], [(0.114)], [(0.090)], [(0.077)],
[Common Language], [], [0.926\*\*\*], [], [0.645\*\*\*], [], [0.105],
[], [], [(0.078)], [], [(0.060)], [], [(0.094)],
[Common Currency], [], [0.700\*\*\*], [], [0.716\*\*\*], [], [0.894\*\*\*],
[], [], [(0.203)], [], [(0.134)], [], [(0.126)],
[FTA\/WTO], [], [1.830\*\*\*], [], [1.673\*\*\*], [], [1.793\*\*\*],
[], [], [(0.099)], [], [(0.074)], [], [(0.063)],
[Num.Obs.], [1106870], [1106870], [1010396], [1010396], [1106870], [1106870],
[R2], [0.482], [0.493], [0.594], [0.611], [0.748], [0.807],
[R2 Adj.], [0.482], [0.493], [0.594], [0.610], [0.748], [0.807],
[FE: iso_o], [X], [X], [X], [X], [X], [X],
[FE: iso_d], [X], [X], [X], [X], [X], [X],
[F\-test], [0.717], [0.648], [0.004], [0.001], [NA], [NA],
[Overdispersion], [NA], [NA], [NA], [NA], [1401462242.471], [1080549913.363],

    // tinytable footer after

    table.footer(
      repeat: false,
      // tinytable notes after
    table.cell(align: left, colspan: 7, text([\+ p \< 0.1, \* p \< 0.05, \*\* p \< 0.01, \*\*\* p \< 0.001])),
    ),
    

  ) // end table

  ]) // end align

] // end block
) // end figure
