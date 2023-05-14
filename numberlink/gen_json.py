
import json
import random
import sys

size = 0
with open('data/size.txt') as f:
    for line in f:
        size = int(line)


def get_index(node):
    return (node[1], size - 1 - node[0])


pipe_list = {}
with open('data/board_raw.txt') as f:
    table = []
    for line in f:
        lst = line.split(' ')
        for i in range(size):
            lst[i] = ord(lst[i]) - ord('0')
        table.append(lst)

    for r in range(size):
        for c in range(size):
            if table[r][c] != 0:
                if table[r][c] in pipe_list:
                    pipe_list[table[r][c]].append((r, c))
                else:
                    pipe_list[table[r][c]] = [(r, c)]

format_pipe_list = []
board = {}

with open('data/board.json', 'w') as f:
    sys.stdout = f

    for key in pipe_list:
        pipe = {}
        pipe["start_end"] = (get_index(pipe_list[key][0]),
                             get_index(pipe_list[key][1]))
        pipe["color"] = (random.randint(0, 256), random.randint(
            0, 256), random.randint(0, 256))
        pipe["path"] = []
        format_pipe_list.append(pipe)

    board["size"] = size
    board["pipe_list"] = format_pipe_list
    board["occupied"] = []
    board["current_pipe"] = format_pipe_list[0]
    board["current_node"] = (-1, -1)

    print(json.dumps(board, indent=3))

table = []
with open('data/solution_raw.txt') as f:
    for line in f:
        lst = line.split(' ')
        for i in range(size):
            lst[i] = ord(lst[i]) - ord('0')
        table.append(lst)

with open('data/solution.json', 'w') as f:
    sys.stdout = f

    row = [-1, 0, 1, 0]
    col = [0, 1, 0, -1]
    occupied_list = []

    def find_path(begin, end):
        res = [get_index(begin)]
        occupied_list.append(get_index(begin))

        prev = begin
        while begin[0] != end[0] or begin[1] != end[1]:
            for i in range(4):
                next_r = begin[0] + row[i]
                next_c = begin[1] + col[i]
                if (next_r == prev[0] and next_c == prev[1]):
                    continue
                if (0 <= next_c and next_c < size and 0 <= next_r and next_r < size):
                    if table[next_r][next_c] == table[begin[0]][begin[1]]:
                        prev = begin
                        begin = (next_r, next_c)
                        res.append(get_index(begin))
                        occupied_list.append(get_index(begin))
                        break
        return res

    counter = 0
    for key in pipe_list:
        format_pipe_list[counter]["path"] = find_path(
            pipe_list[key][0], pipe_list[key][1])
        counter += 1

    board["pipe_list"] = format_pipe_list
    board["occupied"] = occupied_list
    board["current_pipe"] = format_pipe_list[0]
    board["current_node"] = (-1, -1)

    print(json.dumps(board, indent=3))
