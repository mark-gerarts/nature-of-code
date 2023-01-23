<?php

/**
 * Quick and dirty script for the following things:
 * - Add an entry to the asd system definition for each example
 * - Generate a specific README for each example, if one doesn't exist yet
 * - Build the general README table of contents.
 */

const GITHUB_TREE_BASE = 'https://github.com/mark-gerarts/nature-of-code/tree/master/';
const SCREENSHOT_HOST = 'screenshots/';

// Returns the number of the exercise or example. E.g. "Example 1.10" => 10.
function getExerciseNumber(string $full_name): int {
    preg_match('/\.(\d*):|^\d+/', $full_name, $matches);
    return array_pop($matches) ?: -1;
}

function getSortWeight(string $full_name): int {
    $multiplier = strpos($full_name, 'Example') !== false ? 1 : 100;
    $number = getExerciseNumber($full_name);
    return $number * $multiplier;
}

function getSubDirectories(string $directory): array {
    $directories =  glob($directory . '/*', GLOB_ONLYDIR);
    $directories = array_map('basename', $directories);
    usort($directories, function (string $a, string $b) {
        return getSortWeight($a) <=> getSortWeight($b);
    });

    return $directories;
}

function getChapterStructure(): array {
    $excluded_dirs = [
        'screenshots',
        'assets'
    ];

    $dirs = getSubDirectories(__DIR__);
    $dirs = array_filter(
        $dirs,
        function (string $dir) use ($excluded_dirs): bool {
            return !in_array($dir, $excluded_dirs);
        }
    );

    $structure = [];
    foreach ($dirs as $chapter) {
        $exercises = getSubDirectories($chapter);
        $structure[$chapter] = $exercises;
    }

    return $structure;
}

function toAsdfDefinition(array $chapter_structure) {
    $asdf_template = <<<ASDF
(defsystem :nature-of-code
  :version "0.1"
  :author "Mark Gerarts <mark.gerarts@gmail.com>"
  :depends-on (:trivial-gamekit
               :cl-bodge
               :black-tie)
  ;; We get such a messy component definition because the directory structure
  ;; is set up to be easily browsable, with a README for each example.
  :components (%s))

ASDF;

    $module_template = <<<MODULE
               (:module "%s"
                :pathname "%s"
                :components
                (%s))
MODULE;

    $file_template = <<<FILE
                 (:file "%s"
                  :pathname "%s")
FILE;

    $module_definitions = [];
    foreach ($chapter_structure as $chapter => $contents) {
        $module_name = chapterToModuleName($chapter);
        $file_definitions = [];
        foreach ($contents as $exercise) {
            $file = toPackageName($chapter, $exercise);
            $path_name = "{$exercise}/sketch";
            $file_definitions[] = sprintf($file_template, $file, $path_name);
        }
        $module_definitions[] = sprintf(
            $module_template,
            $module_name,
            $chapter,
            trim(implode("\n", $file_definitions))
        );
    }

    return sprintf(
        $asdf_template,
        trim(implode("\n", $module_definitions))
    );
}


function toPackageName(string $chapter, string $exercise): string {
    $subpackage = exerciseToSubPackageName($exercise);
    $module_name = chapterToModuleName($chapter);

    return "nature-of-code.{$module_name}.{$subpackage}";
}

// "Example I.10 - Hello world" => "example-10"
function exerciseToSubPackageName(string $exercise): string {
    $name = explode(':', $exercise)[0];
    $name = str_replace(['.', ' '], '-', $name);
    $parts = explode('-', $name);
    unset($parts[1]);
    $name = implode('-', $parts);
    $name = strtolower($name);

    return $name;
}

// "01. Vectors" => "vectors"
function chapterToModuleName(string $chapter): string {
    $parts = explode(' ', $chapter);
    unset($parts[0]);
    $parts = array_map('strtolower', $parts);
    return implode('-', $parts);
}

function generateAsdFile(array $structure): void {
    $asdf_file = toAsdfDefinition($structure);
    $file_name = __DIR__ . '/nature-of-code.asd';
    file_put_contents($file_name, $asdf_file);
}

function generateToc(array $structure): string {
    $toc = '';
    foreach ($structure as $chapter => $exercises) {
        $chapter_link = GITHUB_TREE_BASE . rawurlencode($chapter);
        $toc .= "- [$chapter]($chapter_link)\n";

        foreach ($exercises as $exercise) {
            $ex_link = $chapter_link . '/' . rawurlencode($exercise);
            $toc .= "  - [$exercise]($ex_link)\n";
        }
    }

    return $toc;
}

function appendToc(string $toc): void {
    $readme = file(__DIR__ .'/README.md');
    $toc_line = array_search("## Table of contents\n", $readme);
    if (!$toc_line) {
        return;
    }

    $start_line = $toc_line + 1;
    $readme = array_slice($readme, 0, $start_line);

    $toc_lines = explode("\n", $toc);
    $toc_lines = array_map(function (string $line): string {
        return $line . "\n";
    }, $toc_lines);

    $readme = array_merge($readme, $toc_lines);
    $readme = implode("", $readme);
    $readme = trim($readme) . "\n";

    file_put_contents(__DIR__ . '/README.md', $readme);
}

function generateReadmes(array $structure): void {
    $screenshots = glob('screenshots/*');
    $screenshots = array_map('basename', $screenshots);

    $readme_template = <<<README
# %s

Run this example: `(%s:start-sketch)`

![%s](%s)
README;


    foreach ($structure as $chapter => $exercises) {
        foreach ($exercises as $exercise) {
            $readme = $chapter . '/' . $exercise . '/' . 'README.md';
            if (file_exists($readme)) {
                continue;
            }
            if (!in_array($exercise . '.gif', $screenshots)) {
                continue;
            }
            $screenshot_url = SCREENSHOT_HOST . rawurlencode($exercise) . '.gif';
            $package_name = toPackageName($chapter, $exercise);

            $contents = sprintf(
                $readme_template,
                $exercise,
                $package_name,
                $exercise,
                $screenshot_url
            );
            file_put_contents($readme, $contents);
        }
    }
}

function main(): void {
    $structure = getChapterStructure();
    generateAsdFile($structure);
    generateReadmes($structure);
    appendToc(generateToc($structure));
}

main();
