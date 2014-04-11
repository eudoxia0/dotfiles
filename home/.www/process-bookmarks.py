import json
import yaml

root = json.loads(open('bookmarks.1.json').read())

delete_fields = ['dateAdded', 'guid', 'id', 'index', 'lastModified', 'root',
                 'type', 'parent', 'charset', 'annos']

def step(node):
    has_children = 'children' in node
    for field in delete_fields:
        if field in node:
            del node[field]
    if has_children:
        node['children'] = [step(child) for child in
                            node['children']]
    return node

data = [step(child) for child in root['children']]

print(yaml.dump(data, default_flow_style=False))
