empty_board = [[None] * 9] * 9

board = [[None, None, None, None, None, None, None, None, None],
         [None, None, None, None, None, None, None, None, None],
         [None, None, None, None, None, None, None, None, None],
         [None, None, None, None, None, None, None, None, None],
         [None, None, None, None, None, None, None, None, None],
         [None, None, None, None, None, None, None, None, None],
         [None, None, None, None, None, None, None, None, None],
         [None, None, None, None, None, None, None, None, None],
         [None, None, None, None, None, None, None, None, None]]

hardboard = [[   1, None, None, None, None, None, None, None, None],
             [   2, None, None, None, None, None, None, None, None],
             [None, None, None,    8,    7, None, None, None, None],
             [None, None, None, None,    9, None, None, None, None],
             [None, None, None, None, None, None, None, None,    5],
             [None, None, None,    7, None, None, None, None,    6],
             [None, None, None, None, None, None, None, None,    7],
             [None, None, None, None, None, None, None, None,    9],
             [None, None, None, None, None, None, None, None,    8]]

# formats a board into a pretty printed string
def format_board(board):
    if board is None:
        return "Impossible"
    else:
        format_row = lambda row: ' '.join('E' if e is None else str(e) for e in row)
        return '\n'.join(format_row(row) for row in board)

def get_rows(board): return board
def get_cols(board): return [[board[r][c] for r in range(9)] for c in range(9)]

# (i, j) is the index of the subgrid, there are 9 total
def get_subgrids(board): return [
        [board[i * 3 + r][j * 3 + c] for r in range(3) for c in range(3)]
        for i in range(3) for j in range(3)
]

# returns a function that checks whether or not
# predicate returns True for all subgroups in the board
# (this is currying)
def check_board(predicate):
    def checker(board):
        rows = get_rows(board)
        cols = get_cols(board)
        subgrids = get_subgrids(board)

        return all(predicate(group) for group in rows + cols + subgrids)
    
    return checker

def valid_group(group): 
    nonempty = [p for p in group if p is not None]
    return len(set(nonempty)) == len(nonempty)

def complete_group(group): 
    nonempty = [p for p in group if p is not None]
    return [*sorted(nonempty)] == [*range(1,10)]

is_valid = check_board(valid_group)
is_complete = check_board(complete_group)

print(is_valid(empty_board))
print(is_complete(empty_board))

# We have to deep copy the board on every edit
# so that the alterations don't stay.
# One optimization is to alter the board for each
# adjacency and then undo the alteration afterwards
def altered_board(board, r, c, n):
    new_board = [[x for x in row] for row in board]
    new_board[r][c] = n
    return new_board

def solve(board):
    if not is_valid(board):
        return None

    if is_complete(board):
        return board

        # Finds the first spot we can add a number to
    first_empty_spot = next(
            (r, c) for r in range(9) for c in range(9) 
            if board[r][c] is None
    )
    (r, c) = first_empty_spot

    # The adjacencies in our graph are the next possible choices.
    # We want to choose which number to put in the empty spot, so
    # we create a new board for each possibility
    adjacencies = (altered_board(board, r, c, n) for n in range(1, 10))
    results = (solve(adj) for adj in adjacencies)

    # Many of the adjacencies will be impossible to solve,
    # so we have to filter it. Pruning optimization strategies use clever
    # algorithms to figure out whether a board is impossible
    # without actually calling solve() on them
    valid_results = (r for r in results if r != None)

    # Return the first valid result, or None if there aren't any
    return next(valid_results, None)

solved = solve(hardboard)
print(format_board(solved))
