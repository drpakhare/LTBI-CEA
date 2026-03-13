# diagram_functions.R — DiagrammeR decision tree and Markov state diagrams

#' Decision tree diagram (grViz DOT syntax)
decision_tree_diagram <- function() {
  DiagrammeR::grViz("
  digraph decision_tree {
    graph [rankdir=TB, fontname='Arial', bgcolor='white', nodesep=0.6, ranksep=0.8]
    node [fontname='Arial', fontsize=10, style=filled]
    edge [fontname='Arial', fontsize=9]

    // Decision node
    D [label='Patient Candidate\\nfor Biologic', shape=square, fillcolor='#FFD700',
       width=1.8, height=0.8]

    // Strategy nodes
    S1 [label='S1: TST Alone', shape=diamond, fillcolor='#2E5090', fontcolor=white, width=1.5]
    S2 [label='S2: IGRA Alone', shape=diamond, fillcolor='#4472C4', fontcolor=white, width=1.5]
    S3 [label='S3: Cy-Tb Alone', shape=diamond, fillcolor='#70AD47', fontcolor=white, width=1.5]
    S4 [label='S4: TST then IGRA', shape=diamond, fillcolor='#ED7D31', fontcolor=white, width=1.5]
    S5 [label='S5: Treat All', shape=diamond, fillcolor='#FF4444', fontcolor=white, width=1.5]

    // Test outcome nodes
    T1P [label='TST +', shape=ellipse, fillcolor='#FFF2CC']
    T1N [label='TST -', shape=ellipse, fillcolor='#E2EFDA']
    T2P [label='IGRA +', shape=ellipse, fillcolor='#FFF2CC']
    T2N [label='IGRA -', shape=ellipse, fillcolor='#E2EFDA']
    T3P [label='Cy-Tb +', shape=ellipse, fillcolor='#FFF2CC']
    T3N [label='Cy-Tb -', shape=ellipse, fillcolor='#E2EFDA']
    T4P [label='TST +', shape=ellipse, fillcolor='#FFF2CC']
    T4N [label='TST -', shape=ellipse, fillcolor='#E2EFDA']
    T4IP [label='IGRA +', shape=ellipse, fillcolor='#FFF2CC']
    T4IN [label='IGRA -', shape=ellipse, fillcolor='#E2EFDA']

    // Action nodes
    TX [label='TB Prophylaxis\\n+ Biologic', shape=box, fillcolor='#D6E4F0', width=1.4]
    NTX [label='No Prophylaxis\\nBiologic Only', shape=box, fillcolor='#FCE4EC', width=1.4]
    TXA [label='TB Prophylaxis\\n(All Patients)\\n+ Biologic', shape=box, fillcolor='#D6E4F0', width=1.6]

    // Terminal nodes (Markov entry)
    M1 [label='Markov\\nModel', shape=doubleoctagon, fillcolor='#E8E8E8', width=1.2]

    // Edges
    D -> {S1 S2 S3 S4 S5}

    S1 -> T1P [label='Se=57%']
    S1 -> T1N [label='Sp=60%']
    S2 -> T2P [label='Se=74%']
    S2 -> T2N [label='Sp=86%']
    S3 -> T3P [label='Se=75%']
    S3 -> T3N [label='Sp=99%']
    S4 -> T4P [label='TST+']
    S4 -> T4N [label='TST-']
    T4P -> T4IP [label='IGRA+']
    T4P -> T4IN [label='IGRA-']

    T1P -> TX
    T2P -> TX
    T3P -> TX
    T4IP -> TX

    T1N -> NTX
    T2N -> NTX
    T3N -> NTX
    T4N -> NTX
    T4IN -> NTX

    S5 -> TXA

    TX -> M1
    NTX -> M1
    TXA -> M1
  }
  ")
}

#' Markov state transition diagram
markov_state_diagram <- function() {
  DiagrammeR::grViz("
  digraph markov_states {
    graph [rankdir=LR, fontname='Arial', bgcolor='white', nodesep=0.8, ranksep=1.2]
    node [fontname='Arial', fontsize=10, style=filled, shape=ellipse]
    edge [fontname='Arial', fontsize=8]

    // Health states
    W [label='Well on\\nBiologic', fillcolor='#70AD47', fontcolor=white, width=1.3]
    LU [label='LTBI\\nUntreated', fillcolor='#ED7D31', fontcolor=white, width=1.3]
    LP [label='LTBI on\\nProphylaxis', fillcolor='#4472C4', fontcolor=white, width=1.3]
    ATB [label='Active\\nTB', fillcolor='#FF4444', fontcolor=white, width=1.2]
    TBT [label='TB\\nTreatment', fillcolor='#FFC000', width=1.2]
    PTB [label='Post-TB\\nRecovery', fillcolor='#BDD7EE', width=1.2]
    D [label='Dead', fillcolor='#808080', fontcolor=white, shape=doublecircle, width=1.0]

    // Transitions
    LU -> ATB [label='Reactivation\\n(high risk)', color='#FF4444', penwidth=2]
    LP -> ATB [label='Reactivation\\n(reduced)', color='#ED7D31', style=dashed]
    LP -> W [label='Prophylaxis\\ncomplete', color='#70AD47']
    ATB -> TBT [label='Diagnosis', color='#FFC000']
    TBT -> PTB [label='Treatment\\nsuccess', color='#70AD47']
    PTB -> W [label='Resume\\nbiologic', color='#4472C4', style=dashed]

    // Death transitions
    W -> D [color='#808080', style=dotted]
    LU -> D [color='#808080', style=dotted]
    LP -> D [color='#808080', style=dotted]
    ATB -> D [label='TB death', color='#FF4444']
    TBT -> D [label='CFR', color='#808080', style=dotted]

    // Self-loops (staying in state)
    W -> W [label='Stable', color='#70AD47', style=dotted]
    LU -> LU [label='Latent', color='#ED7D31', style=dotted]
  }
  ")
}
