#!/usr/bin/env -S uv run --script

# /// script
# dependencies = ["requests==2.*","PyYAML==6.*"]
# ///

import sys
from textwrap import dedent

# import requests
import yaml

def help():
   print(dedent("""
   USAGE: readme-matrix-generator.py <config.yaml>

      <config.yaml>  should contain definitions for 'versions', 'projects', and
                     label-types
   """), file=sys.stderr)

def generate_md_table(config):
   label_types = {item["name"]: item for item in config["label-types"]}
   lyo_versions = {}
   # Iterate through the YAML data and invert the keys and values
   for value, keys in config["versions"].items():
      for key in keys:
         lyo_versions[key] = value
   # print(label_types, file=sys.stderr)
   # print(lyo_versions, file=sys.stderr)

   # print("| Repo | Version | Status | PRs | Bugs | Activity |")
   # print("| ---- | ------- | ------ | --- | ---- | -------- |")
   print("| Repo | Version | Status | PRs | Activity |")
   print("| ---- | ------- | ------ | --- | -------- |")

   for p in config["projects"]:
      project_link = f"[{p['slug']}]({p['link']})"
      badge_color = label_types[lyo_versions[p['version']]]['color']
      version_badge = f"![](https://img.shields.io/badge/Lyo-{p['version']}-{badge_color})"
      if p.get('archived'):
        activity_badge = "![](https://img.shields.io/badge/status-archived-lightgray)"
        ci_badge = "-"
        pr_badge = "-"
        issues_badge = "-"
      else:
        activity_badge = f"![]({p['activity_badge']})" if 'activity_badge' in p else "N/A"
        ci_badge = f"![]({p['ci_badge']})" if 'ci_badge' in p else "N/A"
        pr_badge = f"[![]({p['pr_badge']})]({p['pr_link']})" if 'pr_badge' in p else "N/A"
        issues_badge = f"[![]({p['issues_badge']})]({p['issues_link']})" if 'issues_badge' in p else "N/A"
      # print(f"| {project_link} | {version_badge} | {ci_badge} | {pr_badge} | {issues_badge} | {activity_badge} |")
      print(f"| {project_link} | {version_badge} | {ci_badge} | {pr_badge} | {activity_badge} |")


if __name__ == "__main__":
   if len(sys.argv) != 2:
      print("YAML config not provided", file=sys.stderr)
      help()
      exit(1)

   with open(sys.argv[1], 'r') as file:
      data = yaml.safe_load(file)
      generate_md_table(data)
