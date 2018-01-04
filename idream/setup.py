from setuptools import setup, find_packages

setup(
    name='idream',
    version='0.1.0',
    maintainer='Eric Conlon',
    maintainer_email='ejconlon@gmail.com',
    packages=find_packages(exclude=['tests*']),
    url='https://github.com/ejconlon/idream',
    description='A simple build system for Idris',
    install_requires=[
        # 'jsonschema'
    ]
)
