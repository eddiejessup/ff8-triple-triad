EVALP = 'p1'

def foo(g, val_floor):
  ngs = g.next_games()
  if EVALP == g.turnp:
    ng_val_max = -100000
    for ng in ngs:
      # - So in this call I'm considering games where I am the player, right.
      # - In the lower call, the other guy will be the player.
      # - I'm considering different moves I make, and the value of the resulting game.
      # - I've gone through a few different moves, and I know the best move so far.
      # - In the end, I'm going to say that the value of this current game, is the
      #   maximum value of the child games, because I get to move, so I'll pick
      #   that one!
      # - For this current next-game, the other guy will pick the move that minimises the value.
      # - If we ever see a move by him, that would result in a game whose value
      #   is less than the best value we've seen so far, we know that we're not
      #   going to pick this current game, so we can stop at that game.
      ng_val = foo(ng, val_floor=ng_val_max)
      if ng_val == 'below_val_floor':
        pass
      elif ng_val > ng_val_max:
        ng_val_max = ng_val
    return ng_val_max
  else:
    ng_val_min = 100000
    for ng in ngs:
      ng_val = foo(ng)
      if ng_val < val_floor:
        return 'below_val_floor'
      elif ng_val < ng_val_min:
        ng_val_min = ng_val
    return ng_val_min
