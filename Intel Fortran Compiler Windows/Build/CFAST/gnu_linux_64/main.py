import socket


def find_free_port():
    temp_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    temp_socket.bind(('localhost', 0))
    _, port = temp_socket.getsockname() 
    temp_socket.close() 

    return port

def save_port(free_port):
    try:
        with open(r"cfast_evac_socket_port.txt", "w+") as f:
            f.write(str(free_port))
            f.close()
    except Exception as e:
        print("Error occurred while saving port:", e)


def start_server():    

    free_port = find_free_port()
    save_port(free_port)

    server_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    server_socket.bind(('localhost', free_port))
    server_socket.listen(1)

    print("Server is waiting for a connection...")

    connection, address = server_socket.accept()
    print(f"Connection established with {address}")

    message_from_client = connection.recv(1024).decode()
    print(f"Received from client: {message_from_client}")
    
    loop_repetition_number = 10
    for time in range(loop_repetition_number-1):

        msg = input("Server msg: ")
        connection.send(msg.encode())
        
        message_from_client = connection.recv(1024).decode()
        print(f"Received from client: {message_from_client}")


    server_socket.close()
if __name__ == "__main__":
    start_server()
