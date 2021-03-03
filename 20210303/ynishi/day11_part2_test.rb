require 'minitest/autorun'
require 'stringio'
require 'pry-byebug'

require_relative './day11_part2'

class Day11Part2Test < Minitest::Test

  def setup
  end

  def ary_to_board(ary)
    build_data(char_array_to_io(ary))
  end

  def char_array_to_io(ary)
    StringIO.new(ary.map(&:join).join("\n"))
  end

  def test_count_occupied_left_to_right
    board_data = [
      %w[. . . . . . .],
      %w[. # . . # . .],
      %w[. . . . . . .],
    ]

    expected = [
      [0, 0, 0, 0, 0, 0, 0],
      [1, 1, 1, 1, 0, 0, 0],
      [0, 0, 0, 0, 0, 0, 0],
    ]

    b = ary_to_board(board_data)
    c = create_counts_array(b)

    count_occupied_left_to_right(b, c)
    assert_equal expected, c
  end

  def test_count_occupied_left_to_right_empty_inserted
    board_data = [
      %w[. . . . . . . . .],
      %w[. L L # . . # . .],
      %w[. . . . . . . . .],
    ]

    expected = [
      [0, 0, 0, 0, 0, 0, 0, 0, 0],
      [0, 0, 1, 1, 1, 1, 0, 0, 0],
      [0, 0, 0, 0, 0, 0, 0, 0, 0],
    ]

    b = ary_to_board(board_data)
    c = create_counts_array(b)

    count_occupied_left_to_right(b, c)
    assert_equal expected, c
  end

  def test_count_occupied_right_to_left
    board_data = [
      %w[. . . . . . .],
      %w[. # . . # . .],
      %w[. . . . . . .],
    ]

    expected = [
      [0, 0, 0, 0, 0, 0, 0],
      [0, 0, 1, 1, 1, 1, 1],
      [0, 0, 0, 0, 0, 0, 0],
    ]

    b = ary_to_board(board_data)
    c = create_counts_array(b)

    count_occupied_right_to_left(b, c)
    assert_equal expected, c
  end

  def test_count_occupied_right_to_left_empty_inserted
    board_data = [
      %w[. . . . . . . .],
      %w[. # . . # L . L],
      %w[. . . . . . . .],
    ]

    expected = [
      [0, 0, 0, 0, 0, 0, 0, 0],
      [0, 0, 1, 1, 1, 1, 0, 0],
      [0, 0, 0, 0, 0, 0, 0, 0],
    ]

    b = ary_to_board(board_data)
    c = create_counts_array(b)

    count_occupied_right_to_left(b, c)
    assert_equal expected, c
  end

  def test_count_occupied_left_to_right_and_right_to_left
    board_data = [
      %w[. . . . . . .],
      %w[. # . . # . .],
      %w[. . . . . . .],
    ]

    expected = [
      [0, 0, 0, 0, 0, 0, 0],
      [1, 1, 2, 2, 1, 1, 1],
      [0, 0, 0, 0, 0, 0, 0],
    ]

    b = ary_to_board(board_data)
    c = create_counts_array(b)

    count_occupied_right_to_left(b, c)
    count_occupied_left_to_right(b, c)

    assert_equal expected, c
  end

  def test_count_occupied_up_to_down
    board_data = [
      %w[. . .],
      %w[. # .],
      %w[. . .],
      %w[. . .],
      %w[. # .],
      %w[. . .],
      %w[. . .],
      %w[. . .],
    ]

    expected = [
      [0, 1, 0],
      [0, 1, 0],
      [0, 1, 0],
      [0, 1, 0],
      [0, 0, 0],
      [0, 0, 0],
      [0, 0, 0],
      [0, 0, 0],
    ]

    b = ary_to_board(board_data)
    c = create_counts_array(b)

    count_occupied_up_to_down(b, c)

    assert_equal expected, c
  end

  def test_count_occupied_up_to_down_empty_inserted
    board_data = [
      %w[. L .],
      %w[. . .],
      %w[. L .],
      %w[. # .],
      %w[. . .],
      %w[. . .],
      %w[. # .],
      %w[. . .],
      %w[. . .],
      %w[. . .],
    ]

    expected = [
      [0, 0, 0],
      [0, 0, 0],
      [0, 1, 0],
      [0, 1, 0],
      [0, 1, 0],
      [0, 1, 0],
      [0, 0, 0],
      [0, 0, 0],
      [0, 0, 0],
      [0, 0, 0],
    ]

    b = ary_to_board(board_data)
    c = create_counts_array(b)

    count_occupied_up_to_down(b, c)

    assert_equal expected, c
  end

  def test_count_occupied_down_to_up
    board_data = [
      %w[. . .],
      %w[. # .],
      %w[. . .],
      %w[. . .],
      %w[. # .],
      %w[. . .],
      %w[. . .],
      %w[. . .],
    ]

    expected = [
      [0, 0, 0],
      [0, 0, 0],
      [0, 1, 0],
      [0, 1, 0],
      [0, 1, 0],
      [0, 1, 0],
      [0, 1, 0],
      [0, 1, 0],
    ]

    b = ary_to_board(board_data)
    c = create_counts_array(b)

    count_occupied_down_to_up(b, c)

    assert_equal expected, c
  end

  def test_count_occupied_down_to_up_empty_inserted
    board_data = [
      %w[. . .],
      %w[. # .],
      %w[. . .],
      %w[. . .],
      %w[. # .],
      %w[. L .],
      %w[. . .],
      %w[. L .],
    ]

    expected = [
      [0, 0, 0],
      [0, 0, 0],
      [0, 1, 0],
      [0, 1, 0],
      [0, 1, 0],
      [0, 1, 0],
      [0, 0, 0],
      [0, 0, 0],
    ]

    b = ary_to_board(board_data)
    c = create_counts_array(b)

    count_occupied_down_to_up(b, c)

    assert_equal expected, c
  end

  def test_count_occupied_up_to_down_and_down_to_up
    board_data = [
      %w[. . .],
      %w[. # .],
      %w[. . .],
      %w[. . .],
      %w[. # .],
      %w[. . .],
      %w[. . .],
      %w[. . .],
    ]

    expected = [
      [0, 1, 0],
      [0, 1, 0],
      [0, 2, 0],
      [0, 2, 0],
      [0, 1, 0],
      [0, 1, 0],
      [0, 1, 0],
      [0, 1, 0],
    ]

    b = ary_to_board(board_data)
    c = create_counts_array(b)

    count_occupied_up_to_down(b, c)
    count_occupied_down_to_up(b, c)

    assert_equal expected, c
  end

  def test_count_occupied_right_up
    board_data = [
      %w[. . . . . . .],
      %w[. # . . . . .],
      %w[. . . . . . .],
      %w[. . . # . . .],
      %w[. . . . # . .],
      %w[. # . . . . .],
      %w[. . . . . . .],
    ]

    expected = [
      [0, 0, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 0, 0],
      [1, 0, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 0, 0],
      [0, 0, 1, 0, 0, 0, 0],
      [0, 1, 0, 1, 0, 0, 0],
      [1, 0, 1, 0, 0, 0, 0],
    ]

    b = ary_to_board(board_data)
    c = create_counts_array(b)

    count_occupied_right_up(b, c)

    assert_equal expected, c
  end

  def test_count_occupied_right_up_empty_inserted
    board_data = [
      %w[. . . . . . . . .],
      %w[. . . # . . . . .],
      %w[. . L . . . . . .],
      %w[. L . . . # . . .],
      %w[. . . . . . # . .],
      %w[. . . # . . . . .],
      %w[. . . . . . . . .],
    ]

    expected = [
      [0, 0, 0, 0, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 0, 0, 0, 0],
      [0, 0, 1, 0, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 1, 0, 0, 0, 0],
      [0, 0, 0, 1, 0, 1, 0, 0, 0],
      [0, 0, 1, 0, 1, 0, 0, 0, 0],
    ]

    b = ary_to_board(board_data)
    c = create_counts_array(b)

    count_occupied_right_up(b, c)

    assert_equal expected, c
  end

  def test_count_occupied_left_down
    board_data = [
      %w[. . . . . . .],
      %w[. # . . . . .],
      %w[. . . . . . .],
      %w[. . . # . . .],
      %w[. . . . # . .],
      %w[. # . . . . .],
      %w[. . . . . . .],
    ]

    expected = [
      [0, 0, 1, 0, 0, 0, 1],
      [0, 0, 0, 0, 0, 1, 0],
      [0, 0, 0, 0, 1, 0, 1],
      [0, 0, 0, 1, 0, 1, 0],
      [0, 0, 1, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 0, 0],
    ]

    b = ary_to_board(board_data)
    c = create_counts_array(b)

    count_occupied_left_down(b, c)

    assert_equal expected, c
  end

  def test_count_occupied_left_down_empty_inserted
    board_data = [
      %w[. . . . . . .],
      %w[. # . . . L .],
      %w[. . . . L . .],
      %w[. . . # . . .],
      %w[. . . . # . .],
      %w[. # . . . . .],
      %w[. . . . . . .],
    ]

    expected = [
      [0, 0, 1, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 1, 0, 1],
      [0, 0, 0, 1, 0, 1, 0],
      [0, 0, 1, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 0, 0],
    ]

    b = ary_to_board(board_data)
    c = create_counts_array(b)

    count_occupied_left_down(b, c)

    assert_equal expected, c
  end

  def test_count_occupied_right_up_and_left_down
    board_data = [
      %w[. . . . . . .],
      %w[. # . . . . .],
      %w[. . . . . . .],
      %w[. . . # . . .],
      %w[. . . . # . .],
      %w[. # . . . . .],
      %w[. . . . . . .],
    ]

    expected = [
      [0, 0, 1, 0, 0, 0, 1],
      [0, 0, 0, 0, 0, 1, 0],
      [1, 0, 0, 0, 1, 0, 1],
      [0, 0, 0, 1, 0, 1, 0],
      [0, 0, 2, 0, 0, 0, 0],
      [0, 1, 0, 1, 0, 0, 0],
      [1, 0, 1, 0, 0, 0, 0],
    ]

    b = ary_to_board(board_data)
    c = create_counts_array(b)

    count_occupied_right_up(b, c)
    count_occupied_left_down(b, c)

    assert_equal expected, c
  end

  def test_count_occupied_right_down
    board_data = [
      %w[. . . . . . .],
      %w[. # . . . . .],
      %w[. . . . . . .],
      %w[. . . # . . .],
      %w[. . . . # . .],
      %w[. # . . . . .],
      %w[. . . . . . .],
    ]

    expected = [
      [1, 0, 0, 0, 0, 0, 0],
      [0, 1, 0, 0, 0, 0, 0],
      [0, 0, 1, 0, 0, 0, 0],
      [0, 0, 0, 1, 0, 0, 0],
      [1, 0, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 0, 0],
    ]

    b = ary_to_board(board_data)
    c = create_counts_array(b)

    count_occupied_right_down(b, c)

    assert_equal expected, c
  end

  def test_count_occupied_right_down_empty_inserted
    board_data = [
      %w[. . . . L . .],
      %w[. # . . . L .],
      %w[. . . . . . #],
      %w[. . . # . . .],
      %w[. . . . # . .],
      %w[. # . . . . .],
      %w[. . . . . . .],
    ]

    expected = [
      [1, 0, 0, 0, 0, 0, 0],
      [0, 1, 0, 0, 0, 1, 0],
      [0, 0, 1, 0, 0, 0, 0],
      [0, 0, 0, 1, 0, 0, 0],
      [1, 0, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 0, 0],
    ]

    b = ary_to_board(board_data)
    c = create_counts_array(b)

    count_occupied_right_down(b, c)

    assert_equal expected, c
  end

  def test_count_occupied_left_up
    board_data = [
      %w[. . . . . . .],
      %w[. # . . . . .],
      %w[. . . . . . .],
      %w[. . . # . . .],
      %w[. . . . # . .],
      %w[. # . . . . .],
      %w[. . . . . . .],
    ]

    expected = [
      [0, 0, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 0, 0],
      [0, 0, 1, 0, 0, 0, 0],
      [0, 0, 0, 1, 0, 0, 0],
      [0, 0, 0, 0, 1, 0, 0],
      [0, 0, 0, 0, 0, 1, 0],
      [0, 0, 1, 0, 0, 0, 1],
    ]

    b = ary_to_board(board_data)
    c = create_counts_array(b)

    count_occupied_left_up(b, c)

    assert_equal expected, c
  end

  def test_count_occupied_left_up_empty_inserted
    board_data = [
      %w[. . . . . . .],
      %w[. # . . . . .],
      %w[. . . . . . .],
      %w[. # . # . . .],
      %w[. . L . # . .],
      %w[. # . L . . .],
      %w[. . . . . . .],
    ]

    expected = [
      [0, 0, 0, 0, 0, 0, 0],
      [0, 0, 0, 0, 0, 0, 0],
      [0, 0, 1, 0, 0, 0, 0],
      [0, 0, 0, 1, 0, 0, 0],
      [0, 0, 1, 0, 1, 0, 0],
      [0, 0, 0, 0, 0, 1, 0],
      [0, 0, 1, 0, 0, 0, 1],
    ]

    b = ary_to_board(board_data)
    c = create_counts_array(b)

    count_occupied_left_up(b, c)

    assert_equal expected, c
  end

  def test_count_occupied_right_down_and_left_up
    board_data = [
      %w[. . . . . . .],
      %w[. # . . . . .],
      %w[. . . . . . .],
      %w[. . . # . . .],
      %w[. . . . # . .],
      %w[. # . . . . .],
      %w[. . . . . . .],
    ]
    expected = [
      [1, 0, 0, 0, 0, 0, 0],
      [0, 1, 0, 0, 0, 0, 0],
      [0, 0, 2, 0, 0, 0, 0],
      [0, 0, 0, 2, 0, 0, 0],
      [1, 0, 0, 0, 1, 0, 0],
      [0, 0, 0, 0, 0, 1, 0],
      [0, 0, 1, 0, 0, 0, 1],
    ]

    b = ary_to_board(board_data)
    c = create_counts_array(b)

    count_occupied_right_down(b, c)
    count_occupied_left_up(b, c)

    assert_equal expected, c
  end

  def test_check_happou_all_cells

    board_data = [
      %w[. . . . . . . . . . . . .],
      %w[. L . L . # . # . # . # .],
      %w[. . . . . . . . . . . . .],
    ]

    expected = [
      [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
      [0, 0, 0, 1, 1, 1, 2, 2, 2, 2, 2, 1, 1],
      [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    ]

    b = ary_to_board(board_data)
    c = create_counts_array(b)

    count_occupied_right_to_left(b, c)
    count_occupied_left_to_right(b, c)

    assert_equal expected, c
  end
end
