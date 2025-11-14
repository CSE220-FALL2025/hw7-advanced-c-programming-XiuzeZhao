#include "/hw7.h"

bst_sf* insert_bst_sf(matrix_sf *mat, bst_sf *root) {
    bst_sf *node =malloc(sizeof(bst_sf));
    if(node==NULL)return root;
    //build node
    node->mat=mat;
    node->left_child=NULL;
    node->right_child=NULL;
    //if empty
    if (root==NULL){
        return node;
    }
    // search and creat
    else{
        bst_sf *temp=root;
        while(1){
            if(node->mat->name<temp->mat->name){
                if(temp->left_child)temp=temp->left_child;
                else {
                    temp->left_child=node;
                    return root;
                }
            }
            else {
                if (temp->right_child)temp=temp->right_child;
                else {
                    temp->right_child=node;
                    return root;
                }
            }
        }
    }
}

matrix_sf* find_bst_sf(char name, bst_sf *root) {
    bst_sf* opt=root;
    while(opt&&opt->mat->name!=name){
        if (opt->mat->name>name){
            opt=opt->left_child;
        }
        else{
            opt=opt->right_child;
        }
    }
    if(!opt)return NULL;
    return opt->mat;
}

void free_bst_sf(bst_sf *root) {
    if(root==NULL)return;
    if(root->left_child!=NULL)free_bst_sf(root->left_child);
    if(root->right_child!=NULL)free_bst_sf(root->right_child);
    free(root);
}

matrix_sf* add_mats_sf(const matrix_sf *mat1, const matrix_sf *mat2) {

    matrix_sf *opt= malloc(sizeof(matrix_sf)+(mat1->num_rows)*(mat1->num_cols)*sizeof(int));
    if(opt==NULL)return NULL;
    opt->num_rows = mat1->num_rows;
    opt->num_cols = mat1->num_cols;

    for(int i=0;i<(mat1->num_rows);i++){
        for(int j=0;j<(mat1->num_cols);j++){
            (opt->values)[(i*mat1->num_cols)+j]=(mat1->values)[(i*mat1->num_cols)+j]+(mat2->values)[(i*mat1->num_cols)+j];
        }
    }
    return opt;
}

matrix_sf* mult_mats_sf(const matrix_sf *mat1, const matrix_sf *mat2) {
    matrix_sf *opt= malloc(sizeof(matrix_sf)+(mat1->num_rows)*(mat2->num_cols)*sizeof(int));
    if(opt==NULL)return NULL;
    opt->num_rows = mat1->num_rows;
    opt->num_cols = mat2->num_cols;

    for(int i=0;i<(opt->num_rows);i++){
        for(int j=0;j<(opt->num_cols);j++){
            int temp=0;
            for(int m=0;m<(mat1->num_cols);m++){
                temp+=mat1->values[(i*mat1->num_cols)+m]*mat2->values[(m*mat2->num_cols)+j];
            }
            (opt->values)[(i*opt->num_cols)+j]=temp;
        }
    }
   return opt;
}

matrix_sf* transpose_mat_sf(const matrix_sf *mat) {
    matrix_sf *opt= malloc(sizeof(matrix_sf)+(mat->num_cols)*(mat->num_rows)*sizeof(int));
    if(opt==NULL)return NULL;
    opt->num_rows = mat->num_cols;
    opt->num_cols = mat->num_rows;

    for(int i=0;i<(opt->num_rows);i++){
        for(int j=0;j<(opt->num_cols);j++){
            opt->values[(i*opt->num_cols)+j]=mat->values[(j*mat->num_cols)+i];
        }
    }
    return opt;
}

matrix_sf* create_matrix_sf(char name, const char *expr) {
    char *p=expr;
    int count=0,r,l,temp=0;
    while(*p){//to match rows and cols
        if(*p>='0'&&*p<='9'){
            temp*=10;
            temp+=*p-'0';
        }
        else if(temp&&!count){
            count++;
            r=temp;
            temp=0;
        }
        else if(temp){
            count++;
            l=temp;
            temp=0;
        }
        if(count==2)break;
        p++;
    }
    matrix_sf *opt= malloc(sizeof(matrix_sf)+r*l*sizeof(int));
    if (opt==NULL) return NULL;

    int * valueP=opt->values;
    while(*p){
        if(*p>='0'&&*p<='9'){
            temp*=10;
            temp+=*p-'0';
        }
        else if(temp){
            *valueP=temp;
            temp=0;
            valueP++;
        }
        p++;
    }
    opt->name=name;
    opt->num_rows=r;
    opt->num_cols=l;

    return opt;
}

int map(char ipt){
    switch (ipt){
        case '+':return 1;
        case '*':return 2;
        case '\'':return 3;
        case'(':return 0;
        case')':return 0;
        case'#':return -1;
    }
}

char* infix2postfix_sf(char *infix) {
    //to store operators
    char* stack=malloc(100*sizeof(char));
    char* temp=stack;
    *temp='#';

    //to store out put
    char* opt=malloc(200*sizeof(char));
    char* temp_opt=opt;

    //traverse
    char* p=infix;
    
    // processing
    while(*p!='\0'){
        if (*p>='0'&&*p<='9'){
            *temp_opt=*p;
            temp_opt++;
        }
        else if(*p==' '){
            p++;
            continue;
        }
        else{
            if(*p=='('){
                temp++;
                *temp='(';
            }
            else if(*p==')'){
                while(map(*temp)>=map(*p)){
                    if(*temp=='('){//pop
                        *temp='\0';
                        temp--;
                    }
                    else{
                        *temp_opt=*temp;
                        temp_opt++;
                        *temp='\0';
                        temp--;
                    }
                }
            }
            else{
                while(map(*temp)>=map(*p)){
                    *temp_opt=*temp;
                    temp_opt++;
                    *temp='\0';
                    temp--;
                }
                temp++;
                *temp=*p;
            }
        }
        p++;

    }

    while(map(*temp)>=map('#')){
        if(*temp=='#'){
            free(stack);
            break;
        }
        *temp_opt=*temp;
        temp_opt++;
        *temp='\0';
        temp--;
    }
    *temp_opt='\0';
    return opt;
}

matrix_sf* evaluate_expr_sf(char name, char *expr, bst_sf *root) {
    char* post=infix2postfix_sf(expr);
    char* ini_post=post;
    matrix_sf **stack=malloc(sizeof(matrix_sf*)*200);
    char* ini_stack=stack;

    //temp varibles
    matrix_sf**a,**b;
    while(*post){
        if(*post>='A'&&*post<='Z'){
            *stack=find_bst_sf(*post,root);
            stack++;
        }
        else{
            if(*post=='+'){
                b=(stack-1);
                stack--;
                a=(stack-1);
                *stack=add_mats_sf(*a,*b);
            }
            else if(*post=='*'){
                b=(stack-1);
                stack--;
                a=(stack-1);
                *stack=mult_mats_sf(*a,*b);
            }
            else if(*post=='\''){
                a=(stack-1);
                *stack=transpose_mat_sf(*a);
            }
        }
        post++;
    }
    stack--;
    matrix_sf *opt=*stack;
    free(ini_stack);
    free(ini_post);
    return opt;
}

matrix_sf *execute_script_sf(char *filename) {
    //get script
    char *str = NULL; 
    FILE *file = fopen(filename,'r');
    size_t max_line_size = MAX_LINE_LEN; 
    getline(&str, &max_line_size, file);
    if(!str)return NULL;

    //build binary search tree
    bst_sf *tree=NULL;
    
    char* find;
    char*temp;
    char temp_name;
    matrix_sf *temp_m=NULL;
    while(str){
        find=strchr(str,'[');
        if(find){//define
            temp=str;
            while(temp!='='){
                temp++;
            }
            temp++;
            temp_m=create_matrix_sf(*str,temp);
            insert_bst_sf(temp_m,tree);
        }
        else{//evaluate
            temp=str;
            temp_name=*str;
            while(temp!='='){
                temp++;
            }
            temp++;
            temp_m=evaluate_expr_sf(temp_name,temp,tree);
            insert_bst_sf(temp_m,tree);
        }
        getline(&str, &max_line_size, file);
    }
    free(str);
    fclose(file);
   return temp_m;
}

// This is a utility function used during testing. Feel free to adapt the code to implement some of
// the assignment. Feel equally free to ignore it.
matrix_sf *copy_matrix(unsigned int num_rows, unsigned int num_cols, int values[]) {
    matrix_sf *m = malloc(sizeof(matrix_sf)+num_rows*num_cols*sizeof(int));
    m->name = '?';
    m->num_rows = num_rows;
    m->num_cols = num_cols;
    memcpy(m->values, values, num_rows*num_cols*sizeof(int));
    return m;
}

// Don't touch this function. It's used by the testing framework.
// It's been left here in case it helps you debug and test your code.
void print_matrix_sf(matrix_sf *mat) {
    assert(mat != NULL);
    assert(mat->num_rows <= 1000);
    assert(mat->num_cols <= 1000);
    printf("%d %d ", mat->num_rows, mat->num_cols);
    for (unsigned int i = 0; i < mat->num_rows*mat->num_cols; i++) {
        printf("%d", mat->values[i]);
        if (i < mat->num_rows*mat->num_cols-1)
            printf(" ");
    }
    printf("\n");
}
