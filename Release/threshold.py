



import sys
import numpy as np
import json

def getUser(name):
  user=name.split('-')[0]
  u=str(user)
  try:
    u=str(int(u))
  except Exception:
    pass
  return u



jsonfile = sys.argv[1]
thresholds = [float(sys.argv[i]) for i in range(2,len(sys.argv))]

with open(jsonfile) as json_file:
    data = json.load(json_file)

artifacts=data['report']['artifacts']


# Probabilities
probas_per_trace = next(tab['value'] for tab in artifacts if tab['name']=="APAttack/probabilities")

print(len(probas_per_trace))
print("threshold,average_precision,average_false_positif")
for t in thresholds:
  precision = 0.0
  false_postive = 0.0
  for trace in probas_per_trace:
    user = getUser(trace)
    probas = probas_per_trace[trace]
    preds =[ pred for pred in probas if probas[pred]>= t ]
    size = len(preds)
    if size != 0 :
      if user in preds:
        precision = precision + 1.0/size
        false_postive = false_postive + (1.0-1.0/size)
      else:
        false_postive = false_postive + 1.0
  precision = precision / len(probas_per_trace)
  false_postive = false_postive / len(probas_per_trace)
  print("%f,%f,%f"%(t,precision,false_postive))













