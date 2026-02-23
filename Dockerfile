
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
ARG JSONF="./build/bin/jsonf"

RUN fpm install --prefix build --profile debug
RUN $JSONF -s '{"a123": 69, "x456": 420}'
RUN $JSONF -s '{"a123": 69, "nestedaoeuaoeuhtnsaoeuhtnsaoehutns": {"p": 1337, "q": 9999}, "x456": 420}'

RUN fpm install --prefix build --profile release
RUN $JSONF -s '{"a123": 69, "x456": 420}'
RUN $JSONF -s '{"a123": 69, "nestedaoeuaoeuhtnsaoeuhtnsaoehutns": {"p": 1337, "q": 9999}, "x456": 420}'

# Cover some cmd args
RUN $JSONF --help
RUN $JSONF --string '{"a123": 69, "nestedaoeuaoeuhtnsaoeuhtnsaoehutns": {"p": 1337, "q": 9999}, "x456": 420}' --tokens
RUN $JSONF --string '{"a123": 69, "nestedaoeuaoeuhtnsaoeuhtnsaoehutns": {"p": 1337, "q": 9999}, "x456": 420}' --compact
RUN $JSONF --string '{"a123": 69, "nestedaoeuaoeuhtnsaoeuhtnsaoehutns": {"p": 1337, "q": 9999}, "x456": 420}' --quiet

# TODO: run these docker tests in debug and release

# Check linting
RUN $JSONF -s '{"a": 1}' --lint  # ok
RUN $JSONF -s '{"a": 1,}' -l # warn trailing comma but ok

# Use '!' to invert the bash exit code. These lints are expected to fail
RUN ! $JSONF -s '{"a": 1,}' -l -Werror=commas
#RUN ! $JSONF -s '{"a": 1,}' -l -Wno-commas  # ok
RUN ! $JSONF -s '{"a": }' -l
RUN ! $JSONF -s '{"a": ' -l
RUN ! $JSONF -s '"unterminated string' -l
RUN ! $JSONF -s '"unterminated string\"' -l
RUN ! $JSONF -s '"unterminated string\" still' -l
RUN ! $JSONF -s '"unterminated string\" still\"' -l
RUN $JSONF -s '"terminated string\" still"' -l
RUN ! $JSONF -s '{"a: 1}' -l
RUN ! $JSONF -s '{"a": 1' -l

# Unterminated array
RUN ! $JSONF -s '[' -l
RUN ! $JSONF -s '{"": [}' -l
RUN $JSONF -s '[]' -l
RUN $JSONF -s '{"": []}' -l

# Invalid numbers
RUN ! $JSONF -s '0.' -l -Werror=numbers
RUN ! $JSONF -s '.0' -l -Werror=numbers
RUN $JSONF -s '0.0' -l -Werror=numbers
RUN $JSONF -s '0.' -l -Wnumbers

# Do one more because I want green text at the end of the log
RUN $JSONF -s '{"a": 1, "b": 2}'

