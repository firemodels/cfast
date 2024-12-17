import socket

def find_free_port():
    # Create temporary socket
    temp_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    temp_socket.bind(('localhost', 0))  # Assigns any available port
    _, port = temp_socket.getsockname()  # Gets the assigned port
    temp_socket.close()  # Closes the temporary socket

    return port

def save_port(free_port):
    try:
        with open(r"cfast_evac_socket_port.txt", "w+") as f:
            f.write(str(free_port))
            f.close()
    except Exception as e:
        print("Error occurred while saving port:", e)

def start_server():

    # Use the function to find a free port
    free_port = find_free_port()
    save_port(free_port)

    server_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    server_socket.bind(('localhost', free_port))
    server_socket.listen(1)

    print("Server is waiting for a connection...")

    connection, address = server_socket.accept()
    print(f"Connection established with {address}")

    #number_of_iterations value must be set each time and must be equal to the &TIME SIMULATION
    #value divided by the &TIME SPREADSHEET value defined in the cfast.in file


    message_from_client = connection.recv(1024).decode()
    print(f"Received from client: {message_from_client}")

    number_of_iterations = 10
    filename = 'doors_opening_level_frame.txt'

    with open(filename, 'a+'):
        pass

    for time in range(number_of_iterations-1):

        msg = input("Server msg: ")

        with open(filename, 'r+') as file:
            file.seek(0)
            file.truncate(0)
            file.write(msg)

        connection.send(msg.encode())

        message_from_client = connection.recv(1024).decode()
        print(f"Received from client: {message_from_client}")

    server_socket.close()

if __name__ == "__main__":
    start_server()