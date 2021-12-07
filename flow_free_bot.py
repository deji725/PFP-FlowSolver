import time
import numpy

def main():
    try:
        board_of_colours =  numpy.array([['0', '0', '0', '0', 'g','0'],
                        ['r', 't', '0', '0','r','g'],
                        ['o', '0', '0', '0', '0','t'],
                        ['y', '0', '0', 'o', '0','b'],
                        ['0', 'b', '0', '0', '0','y'],
                        ['0', '0', '0', '0', '0','0']])
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
    if not impossible_groups_check(board):
        return(False) # this is because we failed the impossible groups check
    return(True)
    # this will check if the board passes all the required constraints with the hope of improving efficiency
    # examples of which are no loops, so all colours must only be adjacent to 1 or 2 of the same colour
    # bottleneck check perhaps
    # this function may not be necessary but should help improve speed
def impossible_groups_check(board):
    characters_that_are_ends = ['b', 'r', 'g', 'y', 'o', 'p', 'z', 'c', 't', 'd', 'q', 's', 'l', 'm', 'w', 'a']
    # this will take in the board and isolate empty spaces into 'groups'
    # it will then check if all of these groups are connected to at least 1 colour with 2 ends in it, else return false
    # it will also check that there are no colours without both its ends in at least one group, else return false
    # failing both of these it will return true
    # This method will group together the empty spaces and check what ends the groups have access to. If a group only has access to both ends of a single colour, then that colour must be the one 
    # to fill the empty space
    group_array = numpy.zeros((board.shape[0], board.shape[1]), dtype=int)
    x = 1
    for i in range(board.shape[0]): # this section creates a new array where groups of empty zones are numbered
        for j in range(board.shape[1]):
            if board[i,j] == '0':
                group_array[i,j] = x
                x += 1
    # next we need to have all the individual groups have unique numbers
    # to do this we will make each group take the same number as the lowest number in its group
    old_array = group_array.copy()
    while(True):
        for i in range(board.shape[0]):
            for j in range(board.shape[1]):
                try:
                    if i > 0:
                        if group_array[i,j] > group_array[i-1,j] and group_array[i-1,j] != 0:
                            group_array[i,j] = group_array[i-1,j]
                except IndexError as e:
                    pass
                try:
                    if group_array[i,j] > group_array[i+1,j] and group_array[i+1,j] != 0:
                        group_array[i,j] = group_array[i+1,j]
                except IndexError as e:
                    pass
                try:
                    if group_array[i,j] > group_array[i,j+1] and group_array[i,j+1] != 0:
                        group_array[i,j] = group_array[i,j+1]
                except IndexError as e:
                    pass    
                try:
                    if j > 0:
                        if group_array[i,j] > group_array[i,j-1] and group_array[i,j-1] != 0:
                            group_array[i,j] = group_array[i,j-1]
                except IndexError as e:
                    pass
        if numpy.array_equal(old_array, group_array):
            break
        else:
            old_array = group_array.copy()
    group_numbers = [] # this will be a list containing the id's of each of the groups
    for number in numpy.unique(group_array):
        if number != 0:
            group_numbers.append(number)
    elements_in_board = numpy.unique(board)
    colours_in_board = []
    for element in  elements_in_board:
        if element in characters_that_are_ends:
            colours_in_board.append(element)
    for group_id in group_numbers:
        connected_ends = []
        temp_unsolved_board = board.copy()
        for i in range(board.shape[0]):
            for j in range(board.shape[1]):
                if group_array[i,j] == group_id:
                    try:
                        if i>0:
                            if temp_unsolved_board[i-1,j] in characters_that_are_ends:
                                connected_ends.append(temp_unsolved_board[i-1,j])
                                temp_unsolved_board[i-1,j] = None
                    except IndexError as e:
                        pass
                    try:
                        if temp_unsolved_board[i+1,j] in characters_that_are_ends:
                            connected_ends.append(temp_unsolved_board[i+1,j])
                            temp_unsolved_board[i+1,j] = None
                    except IndexError as e:
                        pass
                    try:
                        if j>0:
                            if temp_unsolved_board[i,j-1] in characters_that_are_ends:
                                connected_ends.append(temp_unsolved_board[i,j-1])
                                temp_unsolved_board[i,j-1] = None
                    except IndexError as e:
                        pass    
                    try:
                        if temp_unsolved_board[i,j+1] in characters_that_are_ends:
                            connected_ends.append(temp_unsolved_board[i,j+1])
                            temp_unsolved_board[i,j+1] = None
                    except IndexError as e:
                        pass
        colours_with_2_ends_connected = []
        for end in connected_ends[::-1]:
            if connected_ends.count(end) > 1:
                colours_with_2_ends_connected.append(end)
                connected_ends.remove(end)
        if len(colours_with_2_ends_connected)<1:
            return(False) # this is because all groups must have at least 1 colours with both ends connected to it
        for colour in colours_with_2_ends_connected:
            if colour in colours_in_board:
                colours_in_board.remove(colour)
    if len(colours_in_board) > 0:
        return(False) # this is because all colours must have at least 1 group that both its ends are connected to
    return(True)

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