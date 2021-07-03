struct parameter
{
	char* type;
	char* name;
	struct parameter* next;
};

struct data
{
	union
	{
		int ival;
		float fval;
		char* sval;
		char cval;
	}value;
	union
	{
		int* iad;
		float* fad;
		char** sad;
		char* cad;
	}address;
	union
	{
		int* idim;
		float* fdim;
		char** sdim;
		char* cdim;
	}dimensions;
	char* type;
	char* name;
	int line;
	int initialized;
	int isconst;
	int isfunction;
	int dimlimit;
	struct parameter* parameters;
};

struct node
{
    struct data data;
    struct node *next, *child, *parent;
};

typedef struct node node;
typedef struct parameter parameter;

void add_parameter(parameter** pp, char* type, char* name)
{
    while (*pp)
        pp = &(*pp)->next;

    *pp = malloc(sizeof(**pp));
    (*pp)->next = NULL;
    (*pp)->type = malloc(sizeof(type));
	strcpy((*pp)->type, type);
    (*pp)->name = malloc(sizeof(name));
	strcpy((*pp)->name, name);
}

void free_list(parameter* head)
{
 	parameter *n = head;
	while(n)
    {
		parameter *n1 = n;
		n = n->next;
		free(n1);
	}
}

struct data* new_struct(int lineno, char* type, char* name, char* val, int dim, parameter* parm)
{
	struct data* ret = malloc(sizeof(struct data));
	ret->type = malloc(sizeof(type));
	strcpy(ret->type, type);
	ret->name = malloc(sizeof(name));
	strcpy(ret->name, name);
	
	if(strstr(type, "const"))ret->isconst = 1;
	else ret->isconst = 0;
	ret->line = lineno;

	if(parm)
	{
		ret->isfunction = 1;
		parameter *tmp2 = parm;
		while(tmp2)
		{
			add_parameter(&ret->parameters, tmp2->type, tmp2->name);
			tmp2 = tmp2->next;
		}
	}
	else ret->isfunction = 0;

	if(val)
	{
		ret->initialized = 1;
		if(strstr(type, "int") || strstr(type, "bool"))
		{
			ret->value.ival = atoi(val);
			ret->address.iad = &(ret->value.ival);
		}
		else if(strstr(type, "float"))
		{
			ret->value.fval = atof(val);
			ret->address.fad = &(ret->value.fval);
		}
		else if(strstr(type, "string"))
		{
			ret->value.sval = malloc(sizeof(val));
			strcpy(ret->value.sval, val);
			ret->address.sad = &(ret->value.sval);;
		}
		else if(strstr(type, "char"))
		{
			ret->value.cval = atoi(val);
			ret->address.cad = &(ret->value.cval);
		}
	}
	else ret->initialized = 0;

	if(dim)
	{
		ret->dimlimit = dim;
		if(strstr(type, "int") || strstr(type, "bool"))
			ret->dimensions.idim = (int* )malloc(sizeof(int)*dim);
		else if(strstr(type, "float"))
			ret->dimensions.fdim = (float* )malloc(sizeof(float)*dim);
		else if(strstr(type, "string"))
			ret->dimensions.sdim = (char** )malloc(sizeof(char*)*dim);
		else if(strstr(type, "char"))
			ret->dimensions.cdim = (char* )malloc(sizeof(char)*dim);
	}
	else ret->dimlimit = 0;

	return ret;
}

node* new_node(struct data* value)
{
    node *new_node = malloc(sizeof(node));

    if ( new_node )
	{
        new_node->next = NULL;
        new_node->child = NULL;
		new_node->parent = NULL;
        new_node->data = *value;
    }

    return new_node;
}

node* add_sibling(node* n, struct data* value)
{
    if ( n == NULL )
        return NULL;

    while (n->next)
        n = n->next;

    n->next = new_node(value);
	n->next->parent = n->parent;
	return (n->next);
}

node* add_child(node* n, struct data* value) 
{
    if ( n == NULL )
        return NULL;

    if ( n->child )
        return add_sibling(n->child, value);
    else
	{
        n->child = new_node(value);
		n->child->parent = n;
		return (n->child);
	}
}

void delete_tree(node* root)
{
	if(root)
	{
		delete_tree(root->next);
		delete_tree(root->child);
		free(root);
	}
}

int find_in_tree(node* root, char* find)
{
	if(!root) return 0;
	if(strcmp(root->data.name, find) == 0) return root->data.line;
	
	if(find_in_tree(root->next, find) == 1) return root->data.line;

	return find_in_tree(root->child, find);
}

int find_in_branch(node* root, char* find)
{
	if(!root) return 0;
	if(!root->data.isfunction && strcmp(root->data.name, find) == 0) 
	{
		return root->data.line;
	}
	return find_in_branch(root->next, find);
}

int find_variable(node* currentscope, char* name)
{
	if(!currentscope) return 0;

	if(find_in_branch(currentscope->child, name)) return find_in_branch(currentscope->child, name);

	return find_variable(currentscope->parent, name);
}

int findf_in_branch(node* root, char* find, parameter* parm)
{
	if(!root) return 0;
	if(root->data.isfunction && strcmp(root->data.name, find) == 0) 
	{
		parameter* tmp1 = root->data.parameters, *tmp2 = parm;
		while(tmp1 && tmp2)
		{
			if(strcmp(tmp1->type, tmp2->type) == 0) return root->data.line;
			tmp1 = tmp1->next;
			tmp2 = tmp2->next;
		}
		return 0;
	}
	return findf_in_branch(root->next, find, parm);
}

int find_function(node* currentscope, char* name, parameter* parm)
{
	if(!currentscope) return 0;

	if(findf_in_branch(currentscope->child, name, parm)) return findf_in_branch(currentscope->child, name, parm);

	return find_function(currentscope->parent, name, parm);
}

node* get_in_branch(node* root, char* find)
{
	if(!root) return NULL;

	if(!root->data.isfunction && strcmp(root->data.name, find) == 0) return root;

	return get_in_branch(root->next, find);
}

node* get_variable(node* currentscope, char* name)
{
	if(!currentscope) return NULL;

	if(get_in_branch(currentscope->child, name)) return get_in_branch(currentscope->child, name);

	return get_variable(currentscope->parent, name);
}

node* getf_in_branch(node* root, char* find)
{
	if(!root) return NULL;
	if(root->data.isfunction && strcmp(root->data.name, find) == 0) return root;
	
	return getf_in_branch(root->next, find);
}

node* get_function(node* currentscope, char* name)
{
	if(!currentscope) return NULL;

	if(getf_in_branch(currentscope->child, name)) return getf_in_branch(currentscope->child, name);

	return get_function(currentscope->parent, name);
}

node* get_parent_function(node* currentscope)
{
	if(!currentscope) return NULL;

	if(currentscope->data.parameters) return currentscope;

	return get_parent_function(currentscope->parent);
}

node* get_variable_scope(node* currentscope, char* name)
{
	if(!currentscope) return NULL;
	
	if(get_in_branch(currentscope->child, name)) return get_in_branch(currentscope->child, name);

	return get_variable_scope(currentscope->child, name);
} 

node* get_function_scope(node* currentscope, char* name)
{
	if(!currentscope) return NULL;

	if(getf_in_branch(currentscope->child, name)) return getf_in_branch(currentscope->child, name);

	return get_function_scope(currentscope->child, name);
}

void print_tree(FILE* f, node* root)
{
	if(root)
	{
		if((strstr(root->data.type, "int") || strstr(root->data.type, "bool")) && root->data.initialized)
			fprintf(f, "%s %s %d ", root->data.type, root->data.name, root->data.value.ival);
		else if(strstr(root->data.type, "float") && root->data.initialized)
			fprintf(f, "%s %s %f ", root->data.type, root->data.name, root->data.value.fval);
		else if(strstr(root->data.type, "string") && root->data.initialized)
			fprintf(f, "%s %s %s ", root->data.type, root->data.name, root->data.value.sval);
		else if(strstr(root->data.type, "char") && root->data.initialized)
			fprintf(f, "%s %s %c ", root->data.type, root->data.name, root->data.value.cval);
		else fprintf(f, "%s %s ", root->data.type, root->data.name);

		if((strstr(root->data.type, "int") || strstr(root->data.type, "bool")))
			fprintf(f, "%p", root->data.address.iad);
		else if(strstr(root->data.type, "float") && root->data.initialized)
			fprintf(f, "%p", root->data.address.fad);
		else if(strstr(root->data.type, "string") && root->data.initialized)
			fprintf(f, "%p", root->data.address.sad);
		else if(strstr(root->data.type, "char") && root->data.initialized)
			fprintf(f, "%p", root->data.address.cad);

		if(root->data.isfunction)
		{
			fprintf(f, " ( ");
			parameter* tmp = root->data.parameters;
			while(tmp)
			{
				fprintf(f, "%s %s ", tmp->type, tmp->name);
				tmp = tmp->next;
			}
			fprintf(f, ")");
		}
		fprintf(f, "\n");

		if(root->child)fprintf(f, "{\n");
		print_tree(f, root->child);
		if(root->child)fprintf(f, "}\n");
		print_tree(f, root->next);
	}
}

void copy_list ( struct parameter **s, struct parameter* q )
{
    if ( q != NULL )
    {
        *s = malloc ( sizeof ( struct parameter ) ) ;

		(*s)->type = malloc(sizeof(q->type));
        strcpy(( *s ) -> type, q -> type);
		(*s)->name = malloc(sizeof(q->name));
        strcpy(( *s ) -> name, q -> name);
        ( *s ) -> next = NULL ;

        copy_list ( &( ( *s ) -> next ), q->next ) ;
    }
}

int list_size(parameter* item)
{
  struct parameter* cur = item;
  int size = 0;

  while (cur != NULL)
  {
    ++size;
    cur = cur->next;
  }

  return size;
}

int list_size2(node* item)
{
	 struct node* cur = item;
	  int size = 0;

	  while (cur != NULL)
	  {
		if(strcmp(cur->data.type, "functioncall") != 0)++size;
		cur = cur->next;
	  }

	  return size;
}

int strrem(char *src, char *key)
{
	while( *src )
	{
		char *k=key,*s=src;
		while( *k && *k==*s ) ++k,++s;
		if( !*k )
		{
		  while( *s ) *src++=*s++;
		  *src=0;
		  return 1;
		}
		++src;
	}
  return 0;
}

char *strrev(char *str)
{
      char *p1, *p2;

      if (! str || ! *str)
            return str;
      for (p1 = str, p2 = str + strlen(str) - 1; p2 > p1; ++p1, --p2)
      {
            *p1 ^= *p2;
            *p2 ^= *p1;
            *p1 ^= *p2;
      }
      return str;
}
