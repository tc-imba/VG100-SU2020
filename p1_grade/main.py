import os
import shutil
import csv
import jinja2
import subprocess


def minted(filename, firstline, lastline):
    if lastline - firstline <= 25:
        return '\\inputminted[firstline=%d,lastline=%d]{elm}{%s}\n\n' % (firstline, lastline, filename)
    output = '\\inputminted[firstline=%d,lastline=%d]{elm}{%s}\n\n' % (firstline, firstline + 9, filename)
    output += '\\dots\n\n'
    output += '\\inputminted[firstline=%d,lastline=%d]{elm}{%s}\n\n' % (lastline - 9, lastline, filename)
    return output


def main(repo):
    repo_dir = os.path.join('..', 'p1', repo)
    output_dir = os.path.join('.', repo)
    os.makedirs(output_dir, exist_ok=True)
    os.makedirs('./reports', exist_ok=True)
    shutil.copy2('./vg100.sty', os.path.join(output_dir, 'vg100.sty'))

    csv_file = repo + '.csv'
    first = True
    filename = 'other'

    code_quality_info = ''
    code_quality_score = 50

    with open(csv_file) as fin:
        for row in csv.reader(fin):
            if first:
                first = False
                continue
            if not row[2]:
                continue
            if row[0]:
                filename = row[0]
            lines = None
            if row[1]:
                lines = row[1].split('-')
            score = int(row[2])
            description = row[3]

            if score > 0:
                code_quality_info += '%d point(s) {\\color{red}bonus}, %s' % (score, description)
            else:
                code_quality_info += '%d point(s) {\\color{red}deduction}, %s' % (-score, description)

            code_quality_score += score

            if filename.endswith('.elm'):
                src_file = os.path.join(repo_dir, 'src', filename)
                dest_file = os.path.join(output_dir, filename)
                if os.path.exists(src_file):
                    shutil.copy2(src_file, dest_file)
                    # print(lines[0], lines[1])
                    code_quality_info += ', in file {\\color{blue}\\texttt{%s}}, lines {\\color{blue}%s}.\n\n' % (
                    filename, row[1])
                    code_quality_info += minted(filename, int(lines[0]), int(lines[1]))
                else:
                    raise RuntimeError(src_file + ' not exist!')
            else:
                code_quality_info += '.\\medskip\n\n'
                # print(filename)

            # print(filename, lines, score, description)

    template_data = {
        'team': repo[6:],
        'codeQualityInfo': code_quality_info,
        'codeQualityScore': code_quality_score,
    }

    with open(os.path.join('.', 'report.tex')) as file:
        template = jinja2.Template(file.read())

    report_path = os.path.abspath(os.path.join(output_dir, 'report.tex'))
    with open(report_path, 'w') as file:
        file.write(template.render(**template_data))

    os.chdir(output_dir)
    p = subprocess.run('xelatex -synctex=1 -interaction=nonstopmode -shell-escape -8bit report.tex', shell=True,
                       stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    # print(p.stderr)
    os.chdir('..')
    shutil.copy2(os.path.join(output_dir, 'report.pdf'), os.path.join('reports', '%s.pdf' % repo))
    print(repo, code_quality_score)


if __name__ == '__main__':
    for i in range(1, 19):
        main('p1team%d' % i)
