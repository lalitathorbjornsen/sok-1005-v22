# Oppgave 5 b)

import numpy as np

# hvor mange tropper og felter?

# Strategi for spiller 
def player_strategy(n_battalions,n_fields):
    battalions=np.zeros(n_fields,dtype=int)
    
    battalions[0:1]=6 
    battalions[1:4]=30
    battalions[4:]=2
    battalions=battalions[np.random.rand(n_fields).argsort()]
    assert sum(battalions)==n_battalions
    
    return battalions

# Strategi for datamaskinen
def computer_strategy(n_battalions,n_fields):
    battalions=np.zeros(n_fields,dtype=int)
    battalions[0:1]=8
    battalions[1:4]=30
    battalions[4:]=1
    assert sum(battalions)==n_battalions
    return battalions

# En funksjon som gir en poengsum for hvor god en strategi er mot en annen. 
# Følgende funksjon returnerer en poengsum på -1 (tap), 0 (uavgjort) eller 1 (seier):
def call_battle(n_battalions,n_fields, player_strategy, computer_strategy):
    c_battlions=computer_strategy(n_battalions,n_fields)
    p_battlions=player_strategy(n_battalions,n_fields)

    diff=p_battlions-c_battlions
    points=sum(diff>0)-sum(diff<0)
 
    return int(points>0)-int(points<0)

# Følgende funksjon kjærer 100 000
def test_strategies(n_fields,n_battalions,player_strategy, computer_strategy):
    n_tests=100000
    r=0
    record=[]
    for i in range(n_tests):
        p=call_battle(n_battalions,n_fields,
            player_strategy, computer_strategy)
        record.append(p)
        r+=p
    return r/n_tests

r= test_strategies(6,100,player_strategy, computer_strategy)
