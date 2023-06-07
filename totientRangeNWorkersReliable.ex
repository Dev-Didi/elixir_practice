defmodule TotientRange do

  def hcf(x,0), do: x
  def hcf(x,y), do: hcf(y,rem(x,y))

  def relprime(x,y) do
    v = hcf(x,y)
    cond do
      v == 1 -> true
      true -> false
    end
  end

  def euler(n) do
    relprime_n = fn y -> relprime(n,y) end
    length(Enum.filter(Enum.to_list(1..n), relprime_n))
  end

  def print_elapsed(ms,us) do
    us_temp = System.os_time(:microsecond)
    ms2 = System.convert_time_unit(us_temp,:microsecond,:millisecond)
    us2 = us_temp - (ms2 * 100)
    cond do
      us2 - us < 0 ->
        ms3 = ms2 - 1
        us3 = us2 + 1_000_000
        IO.puts("Server: Time taken in MS, microsecs #{ms3-ms} #{us3-us}")
      true ->
        ms3 = ms2
        us3 = us2
        IO.puts("Server: Time taken in MS, microsecs #{ms3-ms} #{us3-us}")
    end
  end

  def sum_totient(lower,upper) do
    Enum.reduce(Enum.map(Enum.to_list(lower..upper),fn x -> euler(x) end),0,fn (x, acc) -> x + acc end)
  end

  def watcher_string(n) do
    "watcher"<>to_string(n)
  end

  def worker_string(n) do
    "worker"<>to_string(n)
  end

  def spawn_watchers(n) do
    watcher_id = String.to_atom(watcher_string(n))
    Process.register(spawn(TotientRange,watcher,[n,0,0]),watcher_id)
    send(watcher_id, {:spawn_worker})
    cond do
      n != 1 ->
        spawn_watchers(n-1)
      true ->
        :ok
    end
  end

  def give_work(:range, lower, interval, n) do
    upper = lower + interval
    watcher_id = String.to_existing_atom(watcher_string(n))
    send(watcher_id,{:range,lower,upper})
    cond do
      n != 1 ->
        give_work(:range, upper+1, interval n-1)
      true ->
        :ok
    end
  end


  def watcher(n,lower,upper) do
    process_flag(:trap_exit,true)
    receive do
      :finished ->
        IO.puts("Watcher#{n}: Finished\n", [n])
      {spawn_worker} ->
        worker_id = String.to_atom(worker_string(n))
        Process.register(spawn(TotientRange,worker,[n]),worker_id)
        IO.puts("Watcher#{n} watching worker#{n}")
        watcher(n,lower,upper)

      {:range,inc_lower,inc_upper} ->
        worker_id = string.to_existing_atom(worker_string(n))
        send(worker_id,{:range,inc_lower,inc_upper})
        watcher(n,inc_lower,inc_upper)

      {'EXIT',from,reason} ->
        IO.puts("Watcher#{n}: #{from} killed by #{reason}. Restarting... \n")
        worker_id = String.to_atom(worker_string(n))
        Process.register(spawn(TotientRange,worker,[n]),worker_id)
        send(worker_id,{:range,lower,upper})

    end

  end
end
