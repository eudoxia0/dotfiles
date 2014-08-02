import json
import yaml

root = json.loads(open('bookmarks.1.json').read())

delete_fields = ['dateAdded', 'guid', 'id', 'index', 'lastModified', 'root',
                 'type', 'parent', 'charset', 'annos']
TITLE_KEY = 'label'

def step(node):
    has_children = 'children' in node or 'sub' in node
    for field in delete_fields:
        if field in node:
            del node[field]
    node[TITLE_KEY] = node['title']
    del node['title']
    if has_children:
        # Folder
        node['sub'] = [step(child) for child in
                       node['children']+node.get('sub',[])]
        del node['children']
    else:
        # Bookmark
        node = {'label': node[TITLE_KEY], 'uri': node['uri']}
    return node

data = [step(child) for child in root['children']]

print(yaml.dump(data, default_flow_style=False, allow_unicode=True))
