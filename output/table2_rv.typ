#show figure: set block(breakable: true)
#figure( // start figure preamble
  
  kind: "tinytable",
  supplement: "Table", // end figure preamble

block[ // start block

#let nhead = 2;
#let nrow = 8;
#let ncol = 10;

  #let style-array = ( 
    // tinytable cell style after
(pairs: ((0, 0), (0, 1), (0, 2), (0, 3), (0, 4), (0, 5), (0, 6), (0, 7), (0, 8), (0, 9),), align: left,),
(pairs: ((1, 0), (1, 1), (1, 2), (1, 3), (1, 4), (1, 5), (1, 6), (1, 7), (1, 8), (1, 9), (2, 0), (2, 1), (2, 2), (2, 3), (2, 4), (2, 5), (2, 6), (2, 7), (2, 8), (2, 9), (3, 0), (3, 1), (3, 2), (3, 3), (3, 4), (3, 5), (3, 6), (3, 7), (3, 8), (3, 9), (4, 0), (4, 1), (4, 2), (4, 3), (4, 4), (4, 5), (4, 6), (4, 7), (4, 8), (4, 9), (5, 0), (5, 1), (5, 2), (5, 3), (5, 4), (5, 5), (5, 6), (5, 7), (5, 8), (5, 9), (6, 0), (6, 1), (6, 2), (6, 3), (6, 4), (6, 5), (6, 6), (6, 7), (6, 8), (6, 9), (7, 0), (7, 1), (7, 2), (7, 3), (7, 4), (7, 5), (7, 6), (7, 7), (7, 8), (7, 9), (8, 0), (8, 1), (8, 2), (8, 3), (8, 4), (8, 5), (8, 6), (8, 7), (8, 8), (8, 9), (9, 0), (9, 1), (9, 2), (9, 3), (9, 4), (9, 5), (9, 6), (9, 7), (9, 8), (9, 9),), align: center,),
  )

  // tinytable align-default-array before
  #let align-default-array = ( left, left, left, left, left, left, left, left, left, left, ) // tinytable align-default-array here
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
    column-gutter: 5pt,
    columns: (auto, auto, auto, auto, auto, auto, auto, auto, auto, auto),
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
 table.hline(y: 2, start: 0, end: 10, stroke: 0.05em + black),
 table.hline(y: 6, start: 0, end: 10, stroke: 0.05em + black),
 table.hline(y: 10, start: 0, end: 10, stroke: 0.1em + black),
 table.hline(y: 0, start: 0, end: 10, stroke: 0.1em + black),
    // tinytable lines before

    table.header(
      repeat: true,
[ ],table.cell(stroke: (bottom: .05em + black), colspan: 9, align: center)[ln GDP],
[ ], [OLS (Log \+ 1)], [OLS (Log \+ 1) ], [OLS (Log \+ 1)  ], [OLS], [OLS ], [OLS  ], [Poisson], [Poisson ], [Poisson  ],
    ),

    // tinytable cell content after
[ln FMP], [0.603\*\*\*], [0.620\*\*\*], [], [0.837\*\*\*], [0.838\*\*\*], [], [0.242\*\*\*], [0.211\*\*\*], [],
[], [(0.001)], [(0.001)], [], [(0.002)], [(0.001)], [], [(0.000)], [(0.000)], [],
[ln FMP (Aug.)], [], [], [0.556\*\*\*], [], [], [0.740\*\*\*], [], [], [0.155\*\*\*],
[], [], [], [(0.001)], [], [], [(0.001)], [], [], [(0.000)],
[Num.Obs.], [1044877], [1044877], [1044877], [1044877], [1044877], [1044877], [1044877], [1044877], [1044877],
[R2], [0.194], [0.443], [0.495], [0.197], [0.435], [0.500], [0.021], [], [],
[R2 Adj.], [0.194], [0.437], [0.489], [0.197], [0.429], [0.494], [0.021], [], [],
[FE: iso_d^year], [], [X], [X], [], [X], [X], [], [X], [X],

    // tinytable footer after

    table.footer(
      repeat: false,
      // tinytable notes after
    table.cell(align: left, colspan: 10, text([\+ p \< 0.1, \* p \< 0.05, \*\* p \< 0.01, \*\*\* p \< 0.001])),
    ),
    

  ) // end table

  ]) // end align

] // end block
) // end figure
