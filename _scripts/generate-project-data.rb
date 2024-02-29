# Preprocessing script
# Run before `jekyll build` to go through _config.yml and use octokit to fill out metadata
#
# Example:
#  - repo: trvrb/coaltrace
#  - owner: trvrb
#  - title: coaltrace
#  - description: Simulating genealogies using charged particles
#  - url: /repos/coaltrace/
#  - date: 2013-10-19 04:12:17 UTC
#  - contributors:
#      - login: trvrb
#      - avatar: https://2.gravatar.com/avatar/ab7fe2db559c7924316c4391ba00b3f0
#      - url: https://github.com/trvrb
#  - commits:
#      - date: 2013-10-19T04:12:06Z
#      - message: Update readme.
#      - url: https://github.com/trvrb/coaltrace/commit/ebb95806f989d8fd6ecbf6aa8308647298dd21ad

require 'octokit'
require 'yaml'

module Repositories

	def self.generate_data(config_file)

		repo_data = {}

		config = YAML.load_file(config_file)
		repos_array = config["repos"]

		puts "Generating repos"
		# create octokit client
		client = Octokit::Client.new(:netrc => true, :access_token => ENV['GITHUB_TOKEN'])

		repo_data = Array.new
		if repos_array.length > 0
			repos_array.each do |repo|

				puts "\tGenerating #{repo}"

				# load repo metadata
				octokit_repo = client.repository(repo)
				repo_title = octokit_repo.name
				repo_owner = octokit_repo.owner.login
				repo_description = octokit_repo.description
				repo_url = "/repos/#{repo_title}/"
				repo_date = octokit_repo.updated_at

				# load contributor metadata
				octokit_contributors = client.contributors(repo)
				repo_contributors = Array.new
				for i in 0 ... [octokit_contributors.size, 5].min
					contributor = octokit_contributors[i]
					contributor_login = contributor.login
					contributor_avatar = contributor.rels[:avatar].href + "&s=50"
					contributor_url = contributor.rels[:html].href
					repo_contributors = repo_contributors.push(
						"login" => contributor_login,
						"avatar" => contributor_avatar,
						"url" => contributor_url
					)
				end

				# load commit metadata
				octokit_commits = client.commits(repo)
				repo_commits = Array.new
				counter = 0
				for i in 0 ... octokit_commits.size
					commit = octokit_commits[i]
					commit_date = commit.commit.author.date
					commit_message = commit.commit.message
					commit_url = commit.rels[:html].href

					if commit.author != nil
						commit_author_login = commit.author.login
						commit_author_url = commit.author.rels[:html].href
					else
						commit_author_login = ""
						commit_author_url = ""
					end

					repo_commits = repo_commits.push(
						"date" => commit_date,
						"message" => commit_message,
						"url" => commit_url,
						"author_login" => commit_author_login,
						"author_url" => commit_author_url
					)
					counter += 1
					if counter == 5
						break
					end

				end

				# assemble metadata
				repo_data = repo_data.push(
					"repo" => repo,
					"title" => repo_title,
					"owner" => repo_owner,
					"description" => repo_description,
					"url" => repo_url,
					"contributors" => repo_contributors,
					"commits" => repo_commits
				)

				# sort by date
				repo_data.sort! { |x, y| y["commits"].first["date"] <=> x["commits"].first["date"] }

			end
		end

		return repo_data

	end

	def self.write_data(repo_data, data_file)

		puts "Writing repo data"
		File.write(data_file, repo_data.to_yaml)

	end

end

repo_data = Repositories.generate_data("_config.yml")
Repositories.write_data(repo_data, "_data/repos.yml")
