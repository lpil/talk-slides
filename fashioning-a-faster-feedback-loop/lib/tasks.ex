defmodule Mix.Tasks.Breakfast do
  use Mix.Task
  def run(_) do
    Mix.Task.run "eggs"
    Mix.Task.run "toast"
    IO.puts "Breakfast is ready!"
  end
end

defmodule Mix.Tasks.Eggs do
  use Mix.Task
  def run(_) do
    Mix.Task.run "buy_ingredients"
    IO.puts "Cooking eggs"
  end
end

defmodule Mix.Tasks.Toast do
  use Mix.Task
  def run(_) do
    Mix.Task.run "buy_ingredients"
    IO.puts "Toasting bread"
  end
end

defmodule Mix.Tasks.BuyIngredients do
  use Mix.Task
  def run(_) do
    IO.puts "Buying ingredients"
  end
end
