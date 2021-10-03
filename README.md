# behave

`behave` is a language for writing Microsoft Flight Simulator Model Behaviors.

It converts the hard-to-understand XML into a declarative language, with a powerful, imperative template system and RPN compiler.

## Why?

`behave` was designed to replace Microsoft Flight Simulator ModelBehavior XML for many reasons:
1. XML is a terrible language to write 'code' in.
2. RPN is a relic from the early 2000s, and is extremely difficult to understand due to its stack-based nature.
3. The error reporting in the XML compiler is terrible. No really. If an error is in an included file, 
good luck figuring out which one. Oh? you wanted a line number? LOL NO.

## Getting Started

1. Download the latest version of `behave` from the [releases](https://github.com/Synaptic-Simulations/behave/releases).
2. Head over to the [wiki](https://github.com/Synaptic-Simulations/behave/wiki) to learn how to use `behave` in your project.

## Contributing

The `behave` compiler is licensed under the GNU GPLv3.

The current quality of code is extremely terrible, which is why `behave` is still in very early alpha.
Please don't scream at me, I warned you. However, feel free to improve the code, add features, and submit pull requests.
