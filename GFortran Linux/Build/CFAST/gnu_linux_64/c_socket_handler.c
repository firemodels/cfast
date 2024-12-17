#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#define BUFFER_SIZE 3072


int connect_to_socket() {
    int client_socket;
    int _port;
    char* _host = "127.0.0.1";
    char* filename= "cfast_evac_socket_port.txt";
    FILE *file = fopen(filename, "r");

    if (fscanf(file, "%d", &_port) != 1) {
        printf("Error reading number from the file.\n");
        fclose(file);
        return 1; 
    }
    fclose(file);

    if ((client_socket = socket(AF_INET, SOCK_STREAM, 0)) == -1) {
        perror("Error creating socket");
        exit(1);
    }
    
    struct sockaddr_in server_address;
    server_address.sin_family = AF_INET;
    server_address.sin_port = htons(_port);
    if (inet_pton(AF_INET, _host, &server_address.sin_addr) != 1) {
        perror("Invalid address");
        close(client_socket);
        exit(1);
    }

    if (connect(client_socket, (struct sockaddr *)&server_address, sizeof(server_address)) == -1) {
        perror("Error connecting to server");
        close(client_socket);
        exit(1);
    }
    return client_socket;
}

void send_message(int sockfd) {
    char* message = "ok";
    if (send(sockfd, message, strlen(message), 0) == -1) {
        perror("Sending message failed");
        exit(EXIT_FAILURE);
    }
}

char *receive_message(int sockfd) {
    char *buffer = (char *)malloc(BUFFER_SIZE * sizeof(char));
    if (buffer == NULL) {
        perror("Memory allocation failed");
        exit(EXIT_FAILURE);
    }

    ssize_t bytes_received = recv(sockfd, buffer, BUFFER_SIZE - 1, 0);
    if (bytes_received == -1) {
        perror("Receiving message failed");
        free(buffer);
        exit(EXIT_FAILURE);
    } else if (bytes_received == 0) {
        printf("Connection closed by peer.\n");
    } else {
        buffer[bytes_received] = '\0';
    }

    return buffer;
}
void c_free(void *ptr) {
    free(ptr);
}
void close_connection(int sockfd) {
    close(sockfd);
}