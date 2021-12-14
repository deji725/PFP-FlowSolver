import time
import numpy

def main():
    try:
        '''
        #6x6 Mania,lvl 149
        board_of_colours =  numpy.array([['0', '0', '0', '0', '0','g'],
                        ['0', 'y', '0', 'y','0','r'],
                        ['0', '0', '0', 'o', '0','b'],
                        ['0', '0', 'b', '0', 'r','0'],
                        ['0', '0', '0', '0', '0','0'],
                        ['0', 'g', '0', '0', '0','o']])
        '''
        with open('regular_9x9_01.txt') as g:
            data = [line.strip().split() for line in g.readlines()]
        board_of_colours =  numpy.array([numpy.array(xi) for xi in data])
        print(board_of_colours)
        # save the initial board of colours as the head of our tree
        # next generate all the children of the parent and add these as branches to some data structure
        # next pass all these children into is_solved() and if any return True then we have a solution
        # if not then generate a new generation of children using all our current children as the parents (this will grow exponentially and need serious optimisation)
        start = time.time()
        solved_board = recurse_solve(board_of_colours, start)
        print('time taken was {} seconds'.format(time.time()-start))
        print(solved_board)
    except Exception as e:
        print(e)

def recurse_solve(board, start_time):
    if time.time()-start_time > 10:
        start_time = time.time()
    characters_that_are_ends = ['b', 'r', 'g', 'y', 'o', 'p', 'z', 'c', 't', 'd', 'q', 's', 'l', 'm', 'w', 'a']
    for i in range(board.shape[0]):
        for j in range(board.shape[1]):
            if board[i,j] in characters_that_are_ends:
                try:
                    if i > 0:
                        if board[i-1,j] == board[i,j]:
                            board[i-1,j] = board[i,j].upper()
                            board[i,j] = board[i,j].upper()
                except IndexError as e:
                    pass
                try:
                    if board[i+1,j] == board[i,j]:
                        board[i+1,j] = board[i,j].upper()
                        board[i,j] = board[i,j].upper()
                except IndexError as e:
                    pass
                try:
                    if j > 0:
                        if board[i,j-1] == board[i,j]:
                            board[i,j-1] = board[i,j].upper()
                            board[i,j] = board[i,j].upper()
                except IndexError as e:
                    pass
                try:
                    if board[i,j+1] == board[i,j]:
                        board[i,j+1] = board[i,j].upper()
                        board[i,j] = board[i,j].upper()
                except IndexError as e:
                    pass
    if is_solved(board):
        return(board)
    elif pass_constraints_check(board):
        for possible_board in find_possible_moves(board):
            temp = recurse_solve(possible_board, start_time) ##Parallization can happen here
            if temp is not None:
                return temp

def find_possible_moves(board): # this will take in a board and return a list of all the possible boards this one could be in 1 move
    characters_that_are_ends = ['b', 'r', 'g', 'y', 'o', 'p', 'z', 'c', 't', 'd', 'q', 's', 'l', 'm', 'w', 'a']
    colour_to_look_for = []
    min_choices = 5
    location_of_current_min_choice_end = None, None
    for i in range(board.shape[0]):
        for j in range(board.shape[0]):
            if board[i,j] in characters_that_are_ends:
                number_of_possible_moves = number_of_possible_moves_check(board, i, j)
                if number_of_possible_moves < min_choices:
                    min_choices = number_of_possible_moves
                    location_of_current_min_choice_end = i, j
    i, j = location_of_current_min_choice_end[0], location_of_current_min_choice_end[1]
    list_of_boards = []
    try:
        if board[i+1,j] == '0':
            temp = board.copy()
            temp[i+1,j] = board[i,j]
            temp[i,j] = temp[i,j].upper()
            list_of_boards.append(temp)
    except IndexError as e:
        pass
    try:
        if i > 0:
            if board[i-1,j] == '0':
                temp = board.copy()
                temp[i-1,j] = board[i,j]
                temp[i,j] = temp[i,j].upper()
                list_of_boards.append(temp)
    except IndexError as e:
        pass
    try:
        if j > 0:
            if board[i,j-1] == '0':
                temp = board.copy()
                temp[i,j-1] = board[i,j]
                temp[i,j] = temp[i,j].upper()
                list_of_boards.append(temp)
    except IndexError as e:
        pass
    try:
        if board[i,j+1] == '0':
            temp = board.copy()
            temp[i,j+1] = board[i,j]
            temp[i,j] = temp[i,j].upper()
            list_of_boards.append(temp)
    except IndexError as e:
        pass
    return(list_of_boards)

def number_of_possible_moves_check(board, i, j):
    number_of_moves = 0
    try:
        if board[i+1,j] == '0':
            number_of_moves += 1
    except IndexError as e:
        pass
    try:
        if i > 0:
            if board[i-1,j] == '0':
                number_of_moves += 1
    except IndexError as e:
        pass
    try:
        if j > 0:
            if board[i,j-1] == '0':
                number_of_moves += 1
    except IndexError as e:
        pass
    try:
        if board[i,j+1] == '0':
            number_of_moves += 1
    except IndexError as e:
        pass
    return(number_of_moves)

def is_solved(board): # takes in a board and tells you if it is solved
    elements_in_board = numpy.unique(board)
    if '0' in elements_in_board:
        return(False)
    for element in elements_in_board:
        if element.islower():
            return(False)
    for colour in elements_in_board:
        number_of_ends = 0
        for i in range(board.shape[0]):
            for j in range(board.shape[1]):
                if board[i,j] == colour:
                    adjacent_colours = number_of_neighbours(board, i, j)
                    if adjacent_colours == 1:
                        number_of_ends += 1
                    elif adjacent_colours == 0:
                        return(False)
        if number_of_ends != 2:
            return(False)
    return(True)

def pass_constraints_check(board):
    characters_that_are_ends = ['b', 'r', 'g', 'y', 'o', 'p', 'z', 'c', 't', 'd', 'q', 's', 'l', 'm', 'w', 'a']
    for i in range(board.shape[0]):
        for j in range(board.shape[1]):
            if board[i,j] != '0':
                if number_of_neighbours(board, i, j) > 2:
                    return(False) # this is because we have some 'loops' on our board and these are not allowed
            if board[i,j] in characters_that_are_ends:
                if number_of_empty_neighbours(board, i, j) == 0:
                    return(False) # this is because we have an isolated end with nowhere to go
    return(True)
    # this will check if the board passes all the required constraints with the hope of improving efficiency
    # examples of which are no loops, so all colours must only be adjacent to 1 or 2 of the same colour
    # bottleneck check perhaps
    # this function may not be necessary but should help improve speed

def number_of_neighbours(board, x, y):
    # this will check how many neighbours the x,y square has of the same colour and return it as an int
    number_of_neighbours = 0
    try:
        if x > 0:
            if board[x-1,y].lower() == board[x,y].lower():
                number_of_neighbours += 1
    except IndexError as e:
        pass
    try:
        if board[x+1,y].lower() == board[x,y].lower():
            number_of_neighbours += 1
    except IndexError as e:
        pass
    try:
        if y > 0:
            if board[x,y-1].lower() == board[x,y].lower():
                number_of_neighbours += 1
    except IndexError as e:
        pass
    try:
        if board[x,y+1].lower() == board[x,y].lower():
            number_of_neighbours += 1
    except IndexError as e:
        pass
    return(number_of_neighbours)

def number_of_empty_neighbours(board, x, y):
        # this will check how many neighbours the x,y square has of the same colour and return it as an int
    number_of_empty_neighbours = 0
    try:
        if x > 0:
            if board[x-1,y] == '0':
                number_of_empty_neighbours += 1
    except IndexError as e:
        pass
    try:
        if board[x+1,y] == '0':
            number_of_empty_neighbours += 1
    except IndexError as e:
        pass
    try:
        if y > 0:
            if board[x,y-1] == '0':
                number_of_empty_neighbours += 1
    except IndexError as e:
        pass
    try:
        if board[x,y+1] == '0':
            number_of_empty_neighbours += 1
    except IndexError as e:
        pass
    return(number_of_empty_neighbours)

if __name__ == '__main__':
    main()