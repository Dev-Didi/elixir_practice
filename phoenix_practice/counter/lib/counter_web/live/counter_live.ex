defmodule CounterWeb.CounterLive do
  use Phoenix.LiveView

  def mount(_params,_session,socket) do
    socket = assign(socket, :count, 0) # assign a stateful value, count, to our socket
    {:ok,socket} # return the socket
  end

  def render(assigns) do
    ~L"""
    <h1>Count: <%=@count %></h1>
    <button phx-click="increment"> + </button>
    <button phx-click="decrement"> - </button>
    """
  end

  def handle_event("increment", _, socket) do
    count = socket.assigns.count + 1
    socket = assign(socket, :count, count)
    {:noreply, socket}
  end

  def handle_event("decrement", _, socket) do
    count = socket.assigns.count - 1
    socket = assign(socket, :count, count)
    {:noreply, socket}
  end
end
