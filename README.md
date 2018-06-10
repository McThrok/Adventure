# Adventure

## User manual

### Main menu commands:

- new [path] - start a new game defined in [path]
- load [path] - load game from [path]
- help - show all commands
- quit - exit

### Game commands:
- save [path] - save game in [path]
- inventory - show all items in inventory
- inventory [item] - show description of [item] from inventory
- go [destination] - move to destination
- look - show location's description
- look [object] - show object's description
- take [object] - pick up [object]
- interact [object] - try interact with [bject]
- use [object] on [target] - try use [object] on [target]
- help - show all commands
- quit - exit to menu

### Example Adventure Guide
- stack exec Adventure
- new src/exampleAdventure.txt
- take shovel (optional, only to show take command usage)
- interact doormat
- use key on door
- go inside
- interact chest