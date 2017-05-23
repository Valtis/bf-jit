CXX = g++-6
SECURITY_FLAGS = -fstack-protector-all -fPIE -D_FORTIFY_SOURCE=2
CXXFLAGS = -std=c++1z -Wall -Wextra -Werror -Wshadow -Wundef -Wcast-align -Wsuggest-override $(SECURITY_FLAGS)
CPPFLAGS =
RELEASE_FLAGS = -O2
DEBUG_FLAGS = -g -Og
LDFLAGS =
CFLAGS =
IDIR = -I.


SRC = $(shell find . -name '*.cpp')
OBJ = $(subst .cpp,.o,$(SRC))
HEADER_DEPS = $(wildcard *.d) $(wildcard */*.d)
BIN = bf

.PHONY: clean
.PHONY: all
.PHONY: debug
.PHONY: release

all: debug

release: CXXFLAGS += $(RELEASE_FLAGS)
release: $(BIN)

debug: CXXFLAGS += $(DEBUG_FLAGS)
debug: $(BIN)

verbosedebug: CPPFLAGS += -DDEBUG
verbosedebug: debug


$(BIN): $(OBJ)
	$(CXX) -o $(BIN) $(OBJ) $(CFLAGS) $(CXXFLAGS) $(LDFLAGS)

%.o: %.cpp
	$(CXX) $(IDIR) $(CPPFLAGS) $(CFLAGS) $(CXXFLAGS) -MMD -c $< -o $@


-include $(HEADER_DEPS)

clean:
	find . -type f -name "*.o" -delete
	find . -type f -name "*.d" -delete
	rm $(BIN)
