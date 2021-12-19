import time
import numpy
import sys

def main():
    with open(sys.argv[1],"r") as g:
        data = [[j for j in line.strip()] for line in g.readlines()]
    board_of_colours =  numpy.array([numpy.array(xi) for xi in data])
    characters_that_are_ends = ['b', 'r', 'g', 'y', 'o', 'p', 'z', 'c', 't', 'd', 'q', 's', 'l', 'm', 'w', 'a']

    #print(characters_that_are_ends)
    current_chars = set()
    for i in range(board_of_colours.shape[0]):
        for j in range(board_of_colours.shape[1]):
            if board_of_colours[i,j].islower():
                current_chars.add(board_of_colours[i,j])

    characters_that_are_ends = list(filter(lambda x: x not in current_chars, characters_that_are_ends))
    #print(characters_that_are_ends)

    new_char = {}
    for i in range(board_of_colours.shape[0]):
        for j in range(board_of_colours.shape[1]):
            if board_of_colours[i,j] == '.':
                board_of_colours[i,j] = '0'
            elif board_of_colours[i,j].isupper():
                print(board_of_colours[i,j])
                if new_char.get(board_of_colours[i,j], 0) == 0:
                    new_char[board_of_colours[i,j]] = characters_that_are_ends[0]
                    current_chars.add(characters_that_are_ends[0])
                    characters_that_are_ends.remove(characters_that_are_ends[0])
                    board_of_colours[i,j] = new_char[board_of_colours[i,j]]
                else:
                    board_of_colours[i,j] = new_char[board_of_colours[i,j]]
    
    #print(board_of_colours)
    with open(sys.argv[1],"w") as g:
        for x in board_of_colours:
            strs = ""
            for j in x:
                strs += j
            g.write(f'{strs}\n')


main()