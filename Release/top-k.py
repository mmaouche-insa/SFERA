



import sys
import numpy as np
import json



jsonfile = sys.argv[1]


with open(jsonfile) as json_file:
    data = json.load(json_file)

artifacts=data['report']['artifacts']


# Top k levels
klevels = next(tab['value'] for tab in artifacts if tab['name']=='APAttack/klevel')

print("-------CSV - KLEVELS-------")
print("trace_id,klevel")
for key in klevels:
    print("%s,%s"%(key,klevels[key]))



