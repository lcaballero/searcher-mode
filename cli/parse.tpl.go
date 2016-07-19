package cli

import (
	"github.com/jessevdk/go-flags"
	"os"
	"github.com/lcaballero/Hack-Q3-2016/conf"
)


func ParseArgs(data interface{}) *conf.Config {
	args := &conf.Config{}
	parser := flags.NewParser(args, flags.Default)
	_, err := parser.ParseArgs(os.Args)
	if err != nil {
		os.Exit(1)
	}
	return args
}


