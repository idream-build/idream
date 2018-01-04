from setuptools import setup

setup(
    name='idream',
    version='0.1.0',
    maintainer='Eric Conlon',
    maintainer_email='ejconlon@gmail.com',
    packages=['idream'],
    url='https://github.com/ejconlon/idream',
    description='A simple build system for Idris',
    package_data={
        'idream': ['schemas/*.json'],
    },
    install_requires=[
        # 'jsonschema'
    ]
)
