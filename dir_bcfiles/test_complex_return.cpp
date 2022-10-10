// a minimal example for NFSD with complex return values

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define ERR_FSAL_NO_ERROR 0
#define FSAL_IS_SUCCESS(_status_) ((_status_).major == ERR_FSAL_NO_ERROR)
#define FSAL_IS_ERROR(_status_) (!FSAL_IS_SUCCESS(_status_))

struct fsal_obj_handle {
    int exampleval;
};

static bool temp = true;
void ace_printf(const char *format, ...) {
  char dest[1024 * 16];
  va_list argptr;
  va_start(argptr, format);
  vsprintf(dest, format, argptr);
  va_end(argptr);
  printf("%s", dest);
}


typedef struct fsal_status__ {
	int major;	/*< FSAL status code */
	int minor;		/*< Other error code (usually POSIX) */
} fsal_status_t;

fsal_status_t check_open_permission(struct fsal_obj_handle* obj, bool flag, char* reason) {
	fsal_status_t status = {obj->exampleval, 0};
	return status;
}

fsal_status_t fsal_reopen2(struct fsal_obj_handle *obj,
			   bool check_permission)
{
	fsal_status_t status = { 0, 0 };
	char reason[]=  "FSAL reopen failed - ";

	if (check_permission) {
		// reproduce the error in vsftpd
		// """ Instruction does not dominate all uses! """
		if (temp) {
			/* Do a permission check on the file before re-opening. */
			status = check_open_permission(obj, temp, reason);
		}
		if (FSAL_IS_ERROR(status))
			goto out;
	}
	status = {1, 1};

 out:
	return status;
}


int main() {
	fsal_obj_handle a = {1};

	fsal_status_t res = fsal_reopen2(&a, true);
    
    return res.major;
}
