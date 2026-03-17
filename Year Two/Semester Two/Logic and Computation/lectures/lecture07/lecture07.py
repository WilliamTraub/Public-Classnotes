#Code 2
def sorted (ys):
  for i in range(0, len(ys)):
    for j in range(i, len(ys)):
      if not ys[i] <= ys[j]:
        return False
  return True
# Has a lot of redundant checks