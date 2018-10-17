
[![Build Status](https://travis-ci.org/ebranlard/testCI.svg?branch=master)](https://travis-ci.org/ebranlard/testCI)



# Hot to setup Github + Travis-CI + Heroku + ReadTheDocs.io 

Github + Travis-CI + Heroku + ReadTheDocs.io

## Step 1 - Github
Create a github repository, here assumed to be called `REPO`, under the user `USER`:
``` 
http://github.com/USER/REPO.git
```


## Step 2 - Travis-CI
 - Create an account on travis-ci.org by logging in with your github credentials.
 - Select the github repository you created
 - Add a file `.travis.yml` at the root of your github repository. Example to run a python script test.py with two different versions of python is given below. The test script may also be a `make test` command. 
```yaml
language: python
python:
  - "2.7"
  - "3.6"
os:
  - linux
  - osx
  - windows
# command to install requirements
install:
  - pip install numpy
# commands to run test
script: 
  - python test.py
# condition
branches:
  only:
    - master
```
- You can add the following to your `README.md` file to include the build status on your github repository page.
```markdown
    [![Build Status](https://travis-ci.org/USER/REPO.svg?branch=master)](https://travis-ci.org/USER/REPO)
```
- Commit the yaml and README.md file, and push
- Check on github and Travis-Ci site, your test should start soon after your push.


 
## Read the docs
Create an account on Readthedocs.org by logging in with your github credentials.

To initialize the documentation in your repository, create a docs directory, install sphynx, and run the quickstart:

```bash
mkdir docs
cd docs
pip install sphynx
sphynx-quickstart
```

Answer the quickstart questions based on your preferences. I say yes to makefiles. 
Then build the documentation

``` bash
make html
```

Add the source and build documentation to git and commit.
After pushing, and waiting for a bit the documenation should be available.




## Summary

- https://travis-ci.org/ebranlard/testCI

- https://testci-ebranlard.readthedocs.io/
