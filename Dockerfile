
FROM rockylinux:9

RUN dnf update -y && dnf install -y \
	cmake \
	gfortran \
	which

#-------------------------------
# Install fpm from binary
WORKDIR /workdir/
#RUN curl -LO https://github.com/fortran-lang/fpm/releases/download/current/fpm-linux-x86_64
RUN curl -LO https://github.com/fortran-lang/fpm/releases/download/current/fpm-linux-x86_64-gcc-12
RUN mv fpm-linux-x86_64* fpm
RUN chmod +x fpm
RUN mv fpm /usr/local/bin
RUN fpm --version
#-------------------------------

WORKDIR /workdir/jsonf

# The .dockerignore stops ADD from copying build output, e.g. .o files
ADD . .

#RUN cmake -S . -B build && cmake --build build #--verbose
RUN fpm test
RUN fpm test --profile debug
RUN fpm test --profile release

RUN fpm install --prefix . --profile debug
RUN ./bin/jsonf -s '{"a123": 69, "x456": 420}'
RUN ./bin/jsonf -s '{"a123": 69, "nestedaoeuaoeuhtnsaoeuhtnsaoehutns": {"p": 1337, "q": 9999}, "x456": 420}'

RUN fpm install --prefix . --profile release
RUN ./bin/jsonf -s '{"a123": 69, "x456": 420}'
RUN ./bin/jsonf -s '{"a123": 69, "nestedaoeuaoeuhtnsaoeuhtnsaoehutns": {"p": 1337, "q": 9999}, "x456": 420}'

# Cover some cmd args
RUN ./bin/jsonf --help
RUN ./bin/jsonf --string '{"a123": 69, "nestedaoeuaoeuhtnsaoeuhtnsaoehutns": {"p": 1337, "q": 9999}, "x456": 420}' --tokens
RUN ./bin/jsonf --string '{"a123": 69, "nestedaoeuaoeuhtnsaoeuhtnsaoehutns": {"p": 1337, "q": 9999}, "x456": 420}' --compact
RUN ./bin/jsonf --string '{"a123": 69, "nestedaoeuaoeuhtnsaoeuhtnsaoehutns": {"p": 1337, "q": 9999}, "x456": 420}' --quiet

# Check linting
RUN ./bin/jsonf -s '{"a": 1}' --lint  # ok
RUN ./bin/jsonf -s '{"a": 1,}' -l # warn trailing comma but ok

# Use '!' to invert the bash exit code. These lints are expected to fail
RUN ! ./bin/jsonf -s '{"a": 1,}' -l -Werror=commas
#RUN ! ./bin/jsonf -s '{"a": 1,}' -l -Wno-commas  # ok
RUN ! ./bin/jsonf -s '{"a": }' -l
RUN ! ./bin/jsonf -s '{"a": ' -l
RUN ! ./bin/jsonf -s '"unterminated string' -l
RUN ! ./bin/jsonf -s '"unterminated string\"' -l
RUN ! ./bin/jsonf -s '"unterminated string\" still' -l
RUN ! ./bin/jsonf -s '"unterminated string\" still\"' -l
RUN ./bin/jsonf -s '"terminated string\" still"' -l
RUN ! ./bin/jsonf -s '{"a: 1}' -l
RUN ! ./bin/jsonf -s '{"a": 1' -l

# Do one more because I want green text at the end of the log
RUN ./bin/jsonf -s '{"a": 1, "b": 2}'

