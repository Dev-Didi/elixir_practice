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

  def sumTotient(lower,upper) do
    Enum.reduce(Enum.map(Enum.to_list(lower..upper),fn x -> euler(x) end),0,fn (x, acc) -> x + acc end)
  end



end
