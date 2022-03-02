#install.packages("DiagrammeR")
library(DiagrammeR)

grViz(diagram = "digraph flowchart {

      # define node aesthetics
      node [fontname = Arial, shape = rectangle,
        color = DarkSlateBlue,
        style = filled,
        fontcolor = White]
      tab1 [label = '@@1']

      node [fontname = Arial, shape = rectangle,
        color = RoyalBlue,
        style = filled,
        fontcolor = White]
      tab2 [label = '@@2']

      node [fontname = Arial, shape = rectangle,
        color = DodgerBlue,
        style = filled,
        fontcolor = White]
      tab3 [label = '@@3']


      node [fontname = Arial, shape = rectangle,
        color = DodgerBlue,
        style = filled,
        fontcolor = White]
      tab4 [label = '@@4']

      node [fontname = Arial, shape = rectangle,
        color = LightSeaGreen,
        style = filled,
        fontcolor = White]
      tab5 [label = '@@5']

      node [fontname = Arial, shape = rectangle,
        color = LightSeaGreen,
        style = filled,
        fontcolor = White]
      tab6 [label = '@@6']

      node [fontname = Arial, shape = rectangle,
        color = LightSeaGreen,
        style = filled,
        fontcolor = White]
      tab7 [label = '@@7']

      node [fontname = Arial, shape = rectangle,
        color = Bisque,
        style = filled,
        width = 4,
        height = .5,
        fontcolor = White]
      tab8 [label = '@@8']

# set up node layout
      tab1 -> tab2;
      tab2 -> tab3;
      tab2 -> tab4;
      tab4 -> tab5;
      tab4 -> tab6;
      tab4 -> tab7;
      tab5 -> tab8;
      tab6 -> tab8;
      tab7 -> tab8;
      tab3 -> tab8;
}

      [1]: 'cBioPortal Database'
      [2]: 'Studies'
      [3]: 'Clinical Data'
      [4]: 'Molecular Profiles'
      [5]: 'Mutations'
      [6]: 'Fusions'
      [7]: 'CNA'
      [8]: 'Sample IDs'
      ")

