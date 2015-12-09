LIB =	support

SRCS =	util_reshape.c util_convert.c util_pbio.c util_sysinfo.c \
	util_system.c util_timer.c util_backtrace.c util_logging.c \
	util_buffer_pool.c util_mem.c util_db_timings.c

ifeq ($(ARCH), SX)
SRCS =	rtc_sx.o util_reshape.c util_convert.c util_pbio.c util_sysinfo.c \
	util_system.c util_timer.c util_backtrace.c util_logging.c \
	util_buffer_pool.c util_mem.c util_db_timings.c
endif

OBJS := $(SRCS:.c=.o)

.SUFFIXES:
.SUFFIXES: .c .o
%.o: %.c
	$(CC) $(CFLAGS) -c $<

all: $(LIB)

$(LIB): ../lib/lib$(LIB).a 

../lib/lib$(LIB).a: $(OBJS)
	$(AR) $(ARFLAGS) ../lib/lib$(LIB).a $(OBJS) 

ifeq ($(ARCH), SX)
rtc_sx.o: rtc_sx.s
	$(AS) -c rtc_sx.s
endif

ifeq ($(ARCH), ES)
rtc_sx.o: rtc_sx.s
	$(AS) -c rtc_sx.s
endif

clean:
	rm -f $(OBJS)
