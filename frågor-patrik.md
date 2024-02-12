

- Nu när kursen är avklarad hur kommer vårat arbetssätt att se ut?

- Hur ska jag formalisera mer än att skriva upp algorithmreglerna?

- Hur svårt är det att göra ett bevis? Beviset i Reiking et al. är ganska 
  långt.

- Vissa av de nuvarande reglerna är jag inte så nöjd med t.ex. BIND-CASE. 
  Känns som att det blir svårt att motivera completeness.

- Dessutom finns det massor av invarianter som BNF-syntaxen inte garanterar. 
  Exempelis så appliceras min Perceus algorithm endast på det som kallas 
  low-level GRIN.

- Jag funderar på att gå över till de Bruijn index i rapporten. 
  Tänkte använda fölande syntax: 

  * λ →  för en bindelse av en variabel λ x →
  * λ³ → för bindelse av en "variabelnod" λ x₁ x₂ x₃ →
  * λ² C_∷_  → för bindelse av en konstant nod λ _∷_ x₁ x₂ →
  * Sedan referar jag till variabler med syntaxen xₙ där 
    t.ex. x₀ är variabeln som är bunden sist. 
    
Problem med update
------------------

Låt l1 peka på en thunk med reference count 1.

HEAP: 
  l0 -> {1, C[]}
  l1 -> {1, CNat, #100}
  l2 -> {1, FUpTo, l1, l0}

UpTo.sum x15 x16 =
  fetch 0 [1] ; λ x97 →
  (case 0 of
     FUpTo.upTo →
       fetch 1 [2] ; λ x118 →
       fetch 2 [3] ; λ x120 →
       UpTo.upTo 1 0
  ) ; λ x94 x95 x96 →
  case 2 of
    [] → ...
    _∷_ → 
       ...
       dup 1 ; λ () → 
       dup 0 ; λ () → 
       update 4 (_∷_ 1 0)

HEAP: l1 -> {1, _∷_, x1, x2}
       
Men i vissa fall så gör vi onödiga updates som inte ändrar noden.
