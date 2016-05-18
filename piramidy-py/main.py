# coding=utf-8

test_puzzles = ['p1', 'p2', 'p3', 'p4', 'p8', 'p11', 'p12']


def main():
    solver = PyramidSolver()

    for i, puzzle_file in enumerate(test_puzzles):
        print "[%d/%d] Test planszy '%s'... " % (i + 1, len(test_puzzles), puzzle_file),
        puzzle = load(read_file('plansze/' + puzzle_file + '.txt'))
        expected_solution = read_file('plansze/' + puzzle_file + '_roz.txt')

        solution = solver.solve(puzzle)
        if solution == expected_solution:
            print "OK"
        else:
            print "błąd"
            print "\nZnalezione rozwiązanie:"
            print solution
            print "\nOczekiwane rozwiązanie:"
            print expected_solution


def read_file(file_name):
    f = open(file_name, 'r')
    content = f.read()
    f.close()
    return content


class PyramidSolver:
    """
    Solver łamigłówki "Piramidy"
    """
    def __init__(self):
        # rozmiar planszy (liczba pól w rzędzie)
        self.n = 0
        # lista wartości na krawędziach planszy (dane wejściowe łamigłówki)
        self.sides = []
        # plansza
        self.board = []

    def solve(self, puzzle):
        self.n = len(puzzle[0])
        self.sides = puzzle
        self.board = [[0 for x in range(self.n)] for y in range(self.n)]
        if self._solve(0, 0):
            return self._solution_to_str()
        else:
            raise Exception('Plansza nie ma rozwiązania')

    def _solve(self, x, y):
        if self.board[y][x] == 0:
            for pyramid in range(1, self.n + 1):
                if self._can_be_placed(x, y, pyramid):
                    self.board[y][x] = pyramid
                    if self._next_solution(x, y):
                        return True
            self.board[y][x] = 0
            return False
        return self._next_solution(x, y)

    def _next_solution(self, x, y):
        if x == self.n - 1 and y == self.n - 1:
            return True
        elif x == self.n - 1:
            return self._solve(0, y + 1)
        else:
            return self._solve(x + 1, y)

    def _can_be_placed(self, x, y, pyramid):
        """
        Zwraca True jeśli piramida o wysokości 'pyramid' może być umieszczona na polu planszy o współrzędnych (x, y).
        Piramida może być umieszczona na planszy jeżeli:
         * w danym wierszu i kolumnie nie ma już piramidy o tej samej wysokości
         * po umieszczeniu piramidy nie będą naruszone warunki łamigłówki (widoczna będzie odpowiednia liczba piramid)
        """
        return self._is_only_one_in_row_and_column(x, y, pyramid) and self._fulfills_pyramid_condition(x, y, pyramid)

    def _is_only_one_in_row_and_column(self, x, y, pyramid):
        for i in range(self.n):
            if self.board[y][i] == pyramid or self.board[i][x] == pyramid:
                return False
        return True

    def _fulfills_pyramid_condition(self, x, y, pyramid):
        visible_from_top = self.sides[0][x]
        visible_from_bottom = self.sides[1][x]
        visible_from_left = self.sides[2][y]
        visible_from_right = self.sides[3][y]

        if visible_from_top:
            pyramids_from_top = [self.board[i][x] for i in range(self.n)]
            pyramids_from_top[y] = pyramid
            min_visible, max_visible = self._estimate_visible_pyramids(pyramids_from_top)
            if not min_visible <= visible_from_top <= max_visible:
                return False
        if visible_from_bottom:
            pyramids_from_bottom = [self.board[i][x] for i in range(self.n - 1, -1, -1)]
            pyramids_from_bottom[self.n - y - 1] = pyramid
            min_visible, max_visible = self._estimate_visible_pyramids(pyramids_from_bottom)
            if not min_visible <= visible_from_bottom <= max_visible:
                return False
        if visible_from_left:
            pyramids_from_left = [self.board[y][i] for i in range(self.n)]
            pyramids_from_left[x] = pyramid
            min_visible, max_visible = self._estimate_visible_pyramids(pyramids_from_left)
            if not min_visible <= visible_from_left <= max_visible:
                return False
        if visible_from_right:
            pyramids_from_right = [self.board[y][i] for i in range(self.n - 1, -1, -1)]
            pyramids_from_right[self.n - x - 1] = pyramid
            min_visible, max_visible = self._estimate_visible_pyramids(pyramids_from_right)
            if not min_visible <= visible_from_right <= max_visible:
                return False
        return True

    def _count_visible_pyramids(self, lst):
        """
        Zwraca liczbę piramid widocznych na podanej liście (patrząc od lewej strony, czyli od początku listy).
        """
        num_visible = 0
        max_height = 0
        for pyramid in lst:
            if pyramid > max_height:
                num_visible += 1
                max_height = pyramid
        return num_visible

    def _estimate_visible_pyramids(self, lst):
        return self._count_visible_pyramids(self._fill(lst, True)), self._count_visible_pyramids(self._fill(lst))

    def _fill(self, lst, reverse=False):
        lst = lst[:]
        remaining = range(1, self.n + 1) if reverse else range(self.n, 0, -1)
        for i in lst:
            if i != 0:
               remaining.remove(i)
        for i in range(self.n):
            if lst[i] == 0:
                lst[i] = remaining.pop()
        return lst

    def _solution_to_str(self):
        """
        Zwraca tekstową reprezentację rozwiązania.
        """
        return "\n".join(["".join([str(p) for p in row]) for row in self.board])


def load(puzzle):
    """
    Wczytuje łamigłówkę (wartości na krawędziach planszy) na podstawie tekstowej reprezentacji.
    """
    sides = puzzle.split("\n")
    if len(sides) != 4:
        raise Exception("Zagadka musi zawierać wartości z czterech stron planszy (format: \"góra|dół|lewa|prawa\").")
    board_size = len(sides[0])
    if any([len(side) != board_size for side in sides ]):
        raise Exception("Nieprawidłowa plansza (różna liczba wartości przy krawędziach planszy).")
    return [[int(ch) if ch != '-' else None for ch in side] for side in sides]


if __name__ == '__main__':
    main()