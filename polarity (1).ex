defmodule Polarity do

  @moduledoc """
    Add your solver function below. You may add additional helper functions if you desire. 
    Test your code by running 'mix test --seed 0' from the simple_tester_ex directory.
  """

  
  def polarity(board, specs) do
    board_list = Tuple.to_list(board)
    rows = length(board_list)

    cols = String.length(hd(board_list))
    intg = for _ <- 1..rows, do: String.duplicate(".", cols)



  constraints = %{
      left: Tuple.to_list(specs["left"]),
      right: Tuple.to_list(specs["right"]),
      top: Tuple.to_list(specs["top"]),
      bottom: Tuple.to_list(specs["bottom"])
    }
  


    pairs = getp(board_list)
  



    sorted_pairs = Enum.sort_by(pairs, fn {{r1, _c1}, {r2, _c2}, _} ->
      Enum.count(String.graphemes(Enum.at(intg, r1)), &(&1 == ".")) +
      Enum.count(String.graphemes(Enum.at(intg, r2)), &(&1 == "."))
    end)
  




    case s1(sorted_pairs, intg, constraints, 0, MapSet.new()) do

      nil -> {}
      solution ->

        IO.puts("Final Board:")
        Enum.each(solution, &IO.puts/1)  
        List.to_tuple(solution)

    end
  end



    defp getp(board) do
      rows = length(board)
      cols = String.length(hd(board))
  
      for r <- 0..(rows - 1),
          c <- 0..(cols - 1),
          ctile = String.at(Enum.at(board, r), c),
          ctile in ["T", "B", "L", "R"],

          {pr1, pc1} = getcord(r, c, ctile),
          pr1 >= 0 and pr1 < rows and pc1 >= 0 and pc1 < cols do
        {{r, c}, {pr1, pc1}, ctile}
      end
    end

    defp getcord(r, c, tile) do


      case tile do
        "T" -> {r + 1, c}
        "B" -> {r - 1, c}
        "L" -> {r, c + 1}
        "R" -> {r, c - 1}
      end
    end

    defp s1([], grid, constraints, _depth, _visited) do
      if adjaency(grid) && con1(grid, constraints) do
        grid
      else
       nil
      end
    end
    defp s1([pair | rest], grid, constraints, depth, visited) do
      if depth > 100 do
         nil
      else
        sint(pair, rest, grid, constraints, depth, visited)
      end
    end


    defp sint(pair, rest, grid, constraints, depth, visited) do
      {{r1, c1}, {r2, c2}, _} = pair
    
      if depth > 80 do
        
        nil
      else
        if cfill?(grid, r1, c1) or cfill?(grid, r2, c2) do
          s1(rest, grid, constraints, depth, visited)
        else
          try(pair, rest, grid, constraints, depth, visited) 
        end
      end
    end


    defp try(pair, rest, grid, constraints, depth, visited) do
      {{r1, c1}, {r2, c2}, _} = pair
    
      best_moves = [{"+", "-"}, {"-", "+"}, {"X", "X"}]
      |> Enum.shuffle() 
    
      Enum.find_value(best_moves, fn {v1, v2} ->
        new_grid = place_values(grid, r1, c1, r2, c2, v1, v2)
    
        if new_grid && not MapSet.member?(visited, new_grid) && adjaency(new_grid) &&  constraints(new_grid, constraints) do
         
          s1(rest, new_grid, constraints, depth, MapSet.put(visited, new_grid))
        else
          
          nil
        end
      end)
    end


    defp constraints(grid, constraints) do
      row_check = Enum.all?(Enum.with_index(grid), fn {row, r} ->

        String.contains?(row, ".") or chck_line(row, Enum.at(constraints.left, r), Enum.at(constraints.right, r))


      end)
  
      col_check = Enum.all?(0..(String.length(hd(grid)) - 1), fn c ->

        col = Enum.map(grid, &String.at(&1, c)) |> Enum.join()
        String.contains?(col, ".") or chck_line(col, Enum.at(constraints.top, c), Enum.at(constraints.bottom, c))


      end)
  
      row_check && col_check
    end
    defp adjaency(grid) do
      row_check = Enum.all?(grid, fn row ->
        not Enum.any?(Enum.chunk_every(String.graphemes(row), 2, 1, :discard), fn [a, b] ->
          a in ["+", "-"] and a == b
        end)
      end)
    
      col_check = Enum.all?(0..(String.length(hd(grid)) - 1), fn c ->


        column = Enum.map(grid, &String.at(&1, c))
        not Enum.any?(Enum.chunk_every(column, 2, 1, :discard), fn [a, b] ->

          a in ["+", "-"] and a == b
        end)
      end)
    
      row_check && col_check
    end
    defp cfill?(grid, row, col) do
      String.at(Enum.at(grid, row), col) != "."
    end
    defp place_values(grid, r1, c1, r2, c2, v1, v2) do
    
      case update_at(grid, r1, c1, v1) do
        nil -> nil

        grid1 -> update_at(grid1, r2, c2, v2)
      end
    end






    defp update_at(grid, row, col, value) do
      current_row = Enum.at(grid, row)
      if String.at(current_row, col) == "." do



        List.replace_at(grid, row, String.slice(current_row, 0, col) <> value <> String.slice(current_row, col + 1, String.length(current_row) - col - 1))
      else
        nil
      end
    end
    defp con1(grid, constraints) do
      row_check = Enum.all?(Enum.with_index(grid), fn {row, r} ->
        chck_line(row, Enum.at(constraints.left, r), Enum.at(constraints.right, r))


      end)
  

      col_check = Enum.all?(0..(String.length(hd(grid)) - 1), fn c ->
        col = Enum.map(grid, &String.at(&1, c)) |> Enum.join()
        chck_line(col, Enum.at(constraints.top, c), Enum.at(constraints.bottom, c))
      end)
  
      row_check && col_check
    end




    defp chck_line(line, plus_constraint, minus_constraint) do
      plus_count = count_char(line, "+")
      minus_count = count_char(line, "-")




      
      (plus_constraint == -1 or plus_count == plus_constraint) &&
      (minus_constraint == -1 or minus_count == minus_constraint)
    end
  
    defp count_char(string, char) do
      string |> String.graphemes() |> Enum.count(&(&1 == char))
    end
end
