# Here's our simple little watcher GenServer.
#
# We define a public start_link/0 function with which to start the process,
# and implement the `handle_info` callback for the case in which fs sends us a
# file event.
#
# In the case of the event we print the path.
#
defmodule Watcher do
  use GenServer

  # Public API

  def start_link
  do: GenServer.start_link(__MODULE__, [], name: __MODULE__)

  # Callbacks

  def init(_) do
    :ok = Application.ensure_started(:fs)
    :ok = :fs.subscribe
    {:ok, []}
  end

  def handle_info({_pid, {:fs, :file_event}, {path, _event}}, state) do
    IO.puts "file changed: #{path}"
    {:noreply, state}
  end
end

Watcher.start_link()

# ...edit a file...

#=> "file changed: /home/louis/projects/talks/mix-test-watch/4913"
#=> "file changed: /home/louis/projects/talks/mix-test-watch/4913"
#=> "file changed: /home/louis/projects/talks/mix-test-watch/03.exs~"
#=> "file changed: /home/louis/projects/talks/mix-test-watch/03.exs"
#=> "file changed: /home/louis/projects/talks/mix-test-watch/03.exs"

# It works!
