
    push 0
    push ⊥

    while not stack empty
        N ← pop
        I ← pop
        J ← I+1
        if N = ⊥
            N ← next_compat(I+1, O[I].s + O[I].d)
            push I
            push N

            push J
            push ⊥

            push N
            push ⊥
        else
            V[I] = max(O[I].p + V[N], V[J])

    S = [0,⊥]
        N ← ⊥
        I ← 0
        J ← 1
        if N = ⊥
            N ← 2
            push 0
            push 2

            push 1
            push ⊥

            push 2
            push ⊥
        else
            V[I] = max(O[I].p + V[N], V[J])

    S = [0,2,1,⊥,2,⊥]

        N ← ⊥
        I ← 2
        J ← 3
        if N = ⊥
            N ← 4
            push 2
            push 4

            push 3
            push ⊥

            push 4
            push ⊥
        else
            V[I] = max(O[I].p + V[N], V[J])

    S = [0,2,1,⊥,2,4,3,⊥,4,⊥]

