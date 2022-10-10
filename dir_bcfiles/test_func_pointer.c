// a minimal example with function pointers in a struct
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

typedef struct {
    int request_type;
} request_rec;

typedef struct {
    int (*check_authorization)(request_rec *r,
                                        const char *require_line,
                                        const void *parsed_require_line);
} authz_provider;

#define large_non_sense(x) ({\
    (x)=(((23)*x)-x);\
    (x)=(x+2);\
})

struct authz_section_conf {
    const authz_provider *provider;    
};
typedef struct authz_section_conf authz_section_conf;

static int env_check_authorization(request_rec *r,
                                            const char *require_line,
                                            const void *parsed_require_line)
{
    const char *t, *w;

    /* The 'env' provider will allow the configuration to specify a list of
        env variables to check rather than a single variable.  This is different
        from the previous host based syntax. */
    t = require_line;
    
    int result = strcmp(t, "hahahahaha");

    return result;
}

static const authz_provider authz_env_provider =
{
    &env_check_authorization
};


int main() {
    authz_section_conf* section = (authz_section_conf*) malloc(sizeof(authz_section_conf));
    section->provider = &authz_env_provider;

    request_rec r = {.request_type= 1 };
    int res = section->provider->check_authorization(&r, "hahahahah", "haha");
    printf("check_authorization_result = %d\n", res) ;
    if (res != 0) {
        large_non_sense(res);
    }
    free(section);
    
    return 0;
}
