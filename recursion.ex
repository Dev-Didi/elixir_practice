defmodule Recur do

	def print_multiple(msg, n) when n > 0 do 
		IO.puts(msg) 
		print_multiple(msg,n-1)
	end

	def print_multiple(msg, 0), do: :ok
end
