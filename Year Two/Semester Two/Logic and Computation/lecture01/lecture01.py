from typing import List, Tuple
Id = str

def random_senders():
  return
def random_receiver():
  return
def random_amount():
  return

def mk_requests(
  senders: List[Id],
  receiver: Id,
  amount: int
) -> List[Tuple[Id, Id, float]]:
  amount_each = amount / len(senders)
  amount_each = round(amount_each, 2)
  requests = []
  for sid in senders:
    requests.append((sid, receiver, amount_each))
  return requests


def check_property(prop):
  for _ in range(100):
    S = random_senders()
    r = random_receiver()
    a = random_amount()
    if prop(S, r, a) is False:
      return False
  return True

def prop_money_conserved(S, r, a):
  R = mk_requests(S, r, a)
  
  #return sum([x for (fr, to x) in R]) == a
  return