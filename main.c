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
        printf("Invalid argument, to make this program work you should type:\n ./main fileName \n");
        exit(-1);
    }

    //Try to open file
    FILE *inFile = fopen(argv[1], "r");
    if (inFile == NULL)
    {
        printf("Error! Could not open file, to make this program work you should type:\n ./main fileName \n");
        exit(-1);
    }
    remove("cTemp.c");
    preprocessing(argv[1], ancestorsDef);
    char result [(sizeof argv[1])+50];
    sprintf(result, "./result cTemp.c %s", argv[1]);
    command(result);
}