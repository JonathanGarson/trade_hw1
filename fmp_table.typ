#show figure: set block(breakable: true)
#figure( // start figure preamble
  
  kind: "tinytable",
  supplement: "Table", // end figure preamble

block[ // start block

#let nhead = 1;
#let nrow = 16;
#let ncol = 2;

  #let style-array = ( 
    // tinytable cell style after
(pairs: ((0, 0), (0, 1), (0, 2), (0, 3), (0, 4), (0, 5), (0, 6), (0, 7), (0, 8), (0, 9), (0, 10), (0, 11), (0, 12), (0, 13), (0, 14), (0, 15), (0, 16),), align: left,),
(pairs: ((1, 0), (1, 1), (1, 2), (1, 3), (1, 4), (1, 5), (1, 6), (1, 7), (1, 8), (1, 9), (1, 10), (1, 11), (1, 12), (1, 13), (1, 14), (1, 15), (1, 16),), align: center,),
  )

  // tinytable align-default-array before
  #let align-default-array = ( left, left, ) // tinytable align-default-array here
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
    columns: (auto, auto),
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
 table.hline(y: 1, start: 0, end: 2, stroke: 0.05em + black),
 table.hline(y: 11, start: 0, end: 2, stroke: 0.05em + black),
 table.hline(y: 17, start: 0, end: 2, stroke: 0.1em + black),
 table.hline(y: 0, start: 0, end: 2, stroke: 0.1em + black),
    // tinytable lines before

    table.header(
      repeat: true,
[ ], [(1)],
    ),

    // tinytable cell content after
[ln Distance], [\-1.689\*\*\*],
[], [(0.055)],
[Border], [0.841\*\*\*],
[], [(0.155)],
[Common Language], [0.922\*\*\*],
[], [(0.073)],
[Common Currency], [0.300],
[], [(0.199)],
[FTA\/WTO], [0.443\*\*\*],
[], [(0.075)],
[Num.Obs.], [28563],
[R2], [0.747],
[R2 Adj.], [0.743],
[FE: iso_o], [X],
[FE: iso_d], [X],
[F\-test], [0.000],

    // tinytable footer after

    table.footer(
      repeat: false,
      // tinytable notes after
    table.cell(align: left, colspan: 2, text([\+ p \< 0.1, \* p \< 0.05, \*\* p \< 0.01, \*\*\* p \< 0.001])),
    ),
    

  ) // end table

  ]) // end align

] // end block
) // end figure
