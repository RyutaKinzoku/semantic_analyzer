#include <stdio.h>
#include <string.h>
#include "preprocessing.c"

void command(char c []){
    char command[256];
    strcpy(command, c);
    system(command);
}

int main(int argc, char *argv[])
{
    newArray ancestorsDef;
    ancestorsDef.index = 0;
    printf("Welcome to the syntactic analyzer\n");
    if (argc != 2)
    {
        printf("Argumento inválido para hacer funcionar este programa debe ingresar:\n ./main nombreArchivo \n");
        exit(1);
    }

    preprocessing(argv[1], ancestorsDef);
    command("./result cTemp.c");
}